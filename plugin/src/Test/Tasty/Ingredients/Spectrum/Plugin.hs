{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Tasty.Ingredients.Spectrum.Plugin where

import GHC hiding (exprType)
import qualified Data.Map as Map
import System.FilePath as FP
import System.Directory as Dir
import Data.Typeable
import Data.Data

#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins
import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import GHC.HsToCore
#if __GLASGOW_HASKELL__ == 902 || __GLASGOW_HASKELL__ == 900
import GHC.Tc.Utils.Zonk (hsPatType)
#endif
#else
import GhcPlugins
import TcRnTypes
import TcRnMonad
import Desugar
import TcHsSyn
#endif

#if __GLASGOW_HASKELL__ >= 904
import GHC.Hs.Syn.Type (lhsExprType,hsPatType)

getTypeLHsExpr :: LHsExpr GhcTc -> TcM [(SrcSpan, Type)]
getTypeLHsExpr l = return [(getLocA l, lhsExprType l)]

#else

-- Taken from GHCi.UI.Info
getTypeLHsExpr :: LHsExpr GhcTc -> TcM [(SrcSpan, Type)]
getTypeLHsExpr e = do hsc_env <- getTopEnv
                      (_,mbe) <- liftIO $ deSugarExpr hsc_env e
                      return $ case mbe of 
#if __GLASGOW_HASKELL__ == 902
                                Just ce -> [(getLocA e, exprType ce)]
#else
                                Just ce -> [(getLoc e, exprType ce)]
#endif
                                _ -> []

#endif 

-- From GHCi.Info
getTypeLPat :: LPat GhcTc -> TcM [(SrcSpan,Type)]
#if __GLASGOW_HASKELL__ >= 902
getTypeLPat l@(L _ pat) = return [(getLocA l, hsPatType pat)]
#elif __GLASGOW_HASKELL__ >= 810
getTypeLPat l@(L _ pat) = return [(getLoc l, hsPatType pat)]
#elif __GLASGOW_HASKELL__ == 808
getTypeLPat l@(dL ->L _ pat) = return [(getLoc l, hsPatType pat)]
#else
getTypeLPat l@(L _ pat) = return [(getLoc l, hsPatType pat)]
#endif

   
-- | Extract 'Id', 'SrcSpan', and 'Type' for 'LHsBind's
getTypeLHsBind :: LHsBind GhcTc -> TcM [(SrcSpan, Type)]
#if __GLASGOW_HASKELL__ >= 906
getTypeLHsBind (L _spn FunBind{fun_id = pid,fun_matches = MG _ _})
    = return $ [(getLocA pid, varType (unLoc pid))]
#elif __GLASGOW_HASKELL__ >= 902
getTypeLHsBind (L _spn FunBind{fun_id = pid,fun_matches = MG _ _ _})
    = return $ [(getLocA pid, varType (unLoc pid))]
#else
getTypeLHsBind (L _spn FunBind{fun_id = pid,fun_matches = MG _ _ _})
    = return $ [(getLoc pid, varType (unLoc pid))]
#endif

getTypeLHsBind _ = return []


-- helper stolen from @syb@ package
type GenericQ r = forall a. Data a => a -> r

mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
(r `mkQ` br) a = maybe r br (cast a)

-- | Get ALL source spans in the source.
#if __GLASGOW_HASKELL__ >= 902
listifyAllSpans :: Typeable a => LHsBinds GhcTc -> [LocatedA a]
#elif __GLASGOW_HASKELL__ == 808
listifyAllSpans :: (HasSrcSpan a, Typeable a) => LHsBinds GhcTc -> [a]
#else
listifyAllSpans :: Typeable a => LHsBinds GhcTc -> [Located a]
#endif
listifyAllSpans = everythingAllSpans (++) [] ([] `mkQ` (\x -> [x | p x]))
    where
#if __GLASGOW_HASKELL__ >= 902
    p l@(L spn _) = isGoodSrcSpan $ getLocA l
#elif __GLASGOW_HASKELL__ == 808
    p l@(dL -> L spn _) = isGoodSrcSpan $ getLoc l
#else
    p l@(L spn _) = isGoodSrcSpan $ getLoc l
#endif

    -- | Variant of @syb@'s @everything@ (which summarises all nodes
    -- in top-down, left-to-right order) with a stop-condition on 'NameSet's
    everythingAllSpans :: (r -> r -> r) -> r -> GenericQ r -> GenericQ r
    everythingAllSpans k z f x
      | (False `mkQ` (const True :: NameSet -> Bool)) x = z
      | otherwise = foldl k (f x) (gmapQ (everythingAllSpans k z f) x)




plugin :: Plugin
plugin = defaultPlugin {
    typeCheckResultAction = locationTyper,
    pluginRecompile = purePlugin
}



locationTyper :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
locationTyper args mod env = do

    dflags <- getDynFlags

    let binds  = tcg_binds env

    bts <- mapM getTypeLHsBind $ listifyAllSpans binds
    ets <- mapM getTypeLHsExpr $ listifyAllSpans binds
    pts <- mapM getTypeLPat    $ listifyAllSpans binds

    

    let mns = moduleNameString (ms_mod_name mod)
#if __GLASGOW_HASKELL__ == 900 || __GLASGOW_HASKELL__ == 902
        mid = toUnitId $ moduleUnit (ms_mod mod)
#else
        mid = moduleUnitId (ms_mod mod)
#endif
        renderSpan :: SrcSpan -> String
#if __GLASGOW_HASKELL__ >= 900
        renderSpan (RealSrcSpan rsp _) = 
#else
        renderSpan (RealSrcSpan rsp) = 
#endif
            concat [f, ":",ssl, ":", ssc, "-" , sel, ":", sec]
            where ssl = show $ srcSpanStartLine rsp
                  ssc = show $ srcSpanStartCol rsp
                  sel = show $ srcSpanEndLine rsp
                  sec = show $ (srcSpanEndCol rsp -1) -- HpcPoses are weird.
                  f = unpackFS $ srcSpanFile rsp
        hpc_dir = hpcDir dflags
        -- same as for hpc
        hpc_mod_dir | mid == mainUnitId = hpc_dir
                    | otherwise = hpc_dir FP.</> (unitIdString mid)
        rendered = map (\(spn,t)
                        -> (renderSpan spn, [showSDoc dflags $ ppr t]))
                            $ concatMap concat [bts,ets,pts]
    liftIO $ Dir.createDirectoryIfMissing True hpc_mod_dir
    liftIO $ writeFile (hpc_mod_dir FP.</> (mns ++ ".types")) $ show rendered
    return env
