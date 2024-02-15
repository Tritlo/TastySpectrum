{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Tasty.Ingredients.Spectrum.Plugin where

import GHC
import qualified Data.Map as Map
import System.FilePath as FP
import System.Directory as Dir
#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils
import GHC.Iface.Ext.Ast
import GHC.Tc.Types
import GHC.Tc.Utils.Monad
#else
import GhcPlugins
import TcRnTypes
import HieAst
import TcRnMonad
import HieTypes
import HieUtils
#endif



plugin :: Plugin
plugin = defaultPlugin {
    renamedResultAction = keepRenamedSource,
    typeCheckResultAction = locationTyper,
    pluginRecompile = purePlugin
}



locationTyper :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
locationTyper args mod env = do
    let Just rnd = tcg_rn_decls env
        renamed = (rnd , tcg_rn_imports env, tcg_rn_exports env, tcg_doc_hdr env)

    dflags <- getDynFlags
    --TODO: if the hie_file already exists, we don't need to recreate it.
    hsc <- getTopEnv
    HieFile {hie_asts=HieASTs{getAsts = asts},
             hie_types=tys} <- liftIO $ runHsc hsc $ mkHieFile mod env renamed
    let render_ty ti = renderHieType dflags (recoverFullType ti tys)
        hfs = concatMap flattenAst $ Map.elems asts
        renderSpan rsp = concat [f, ":",ssl, ":", ssc, "-" , sel, ":", sec]
            where ssl = show $ srcSpanStartLine rsp
                  ssc = show $ srcSpanStartCol rsp
                  sel = show $ srcSpanEndLine rsp
                  sec = show $ (srcSpanEndCol rsp -1) -- HpcPoses are weird.
                  f = unpackFS $ srcSpanFile rsp
        renderNode :: HieAST TypeIndex -> (String, [String])
#if __GLASGOW_HASKELL__ > 810
        ufs (LexicalFastString fs) = unpackFS fs
        renderNode (Node inf span _)  = (renderSpan span,  renderSourceInfo inf)
        renderSourceInfo (SourcedNodeInfo mp) = case mp Map.!? SourceInfo of
                                                 Just inf -> renderInfo inf
                                                 _ -> []
#else
        ufs = unpackFS
        renderNode (Node inf span _)  = (renderSpan span,  renderInfo inf)
#endif
        renderInfo NodeInfo {nodeType = ty} = map render_ty ty

        rendered = map renderNode hfs

    let mns = moduleNameString (ms_mod_name mod)
        hpc_dir = hpcDir dflags
        -- same as for hpc
        hpc_mod_dir | moduleUnitId (ms_mod mod) == mainUnitId = hpc_dir
                    | otherwise = hpc_dir FP.</> (unitIdString (moduleUnitId $ ms_mod mod))
    liftIO $ Dir.createDirectoryIfMissing True hpc_mod_dir
    liftIO $ writeFile (hpc_mod_dir FP.</> (mns ++ ".types")) $ show rendered
    return env
