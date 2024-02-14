module Test.Tasty.Ingredients.Spectrum.Plugin where

import Control.Lens (universeOf)
import Data.Data.Lens (uniplate)


import GHC.Plugins
import GHC
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Tc.Types
import GHC.Types.SourceText

plugin :: Plugin 
plugin = defaultPlugin {
    typeCheckResultAction = locationTyper 
}


locationTyper args mod env = do
    let fbs = HsValBinds noAnn $ ValBinds mempty (tcg_binds env) []
        fakeLet = noLocA $ HsLet noExtField noHsTok fbs noHsTok xpr
        xpr = noLocA $ HsLit noAnn $ HsChar NoSourceText ' '
        flattenProgram :: LHsExpr GhcTc -> [LHsExpr GhcTc]
        flattenProgram = universeOf uniplate
        allTypes = map (\e -> (getLocA e, lhsExprType e)) $
                        filter (isGoodSrcSpan . getLocA) $
                        flattenProgram fakeLet
        ss :: Outputable p => p -> String
        ss = showSDocUnsafe . ppr

    liftIO $ mapM_ (\(l,t) -> putStrLn $
                        ss l ++ ": " ++ ss t) allTypes


    return env
