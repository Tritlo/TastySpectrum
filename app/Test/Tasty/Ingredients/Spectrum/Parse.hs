{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Tasty.Ingredients.Spectrum.Parse where
import GHC
#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins (mainUnitId)
import GHC.Parser
import GHC.Parser.Lexer
import GHC.Types.SrcLoc
import GHC.Data.EnumSet
import GHC.Data.StringBuffer
import GHC.Data.FastString
import GHC.Utils.Error
import GHC.Utils.Outputable hiding (empty)

#if __GLASGOW_HASKELL__ >= 908
import GHC.Driver.DynFlags (languageExtensions)
#else
import GHC.Driver.Session (languageExtensions)
#endif

import qualified GHC.LanguageExtensions as LangExt
import qualified GHC.Data.EnumSet as ES


parseInfoType :: String -> Either String (HsType GhcPs)
parseInfoType str = case unP parser pst of
                        POk _ a -> Right a
                        PFailed ps ->
                         -- We need dflags to output earlier than 904.
#if __GLASGOW_HASKELL__ >= 904
                           Left $ showSDocUnsafe $ ppr $ errors ps
#else
                           Left ("Unable to parse '" ++ str ++ "'")
#endif
  where defE = ES.insert LangExt.MagicHash $ ES.fromList (languageExtensions Nothing)
#if __GLASGOW_HASKELL__ >= 904
#if __GLASGOW_HASKELL__ < 908
        emptyDiagOpts = DiagOpts empty empty False False Nothing defaultSDocContext
#endif
        flgs = mkParserOpts defE emptyDiagOpts [] False False False False
        pst = initParserState flgs (stringToStringBuffer str) loc
#elif __GLASGOW_HASKELL__ == 902
        flgs = mkParserOpts empty defE False False False False
        pst = initParserState flgs (stringToStringBuffer str) loc
#else
        flgs = mkParserFlags' empty empty mainUnitId False False False False
        pst = mkPStatePure flgs (stringToStringBuffer str) loc
#endif
        loc = mkRealSrcLoc nilFS 0 0
        parser = unLoc <$> parseType
#else
import Parser
import Lexer
import Module
import EnumSet
import StringBuffer
import SrcLoc
import FastString
import GhcPlugins hiding (empty)
import Bag (bagToList)
import DynFlags (languageExtensions)
import qualified EnumSet as ES
import qualified GHC.LanguageExtensions as LangExt

parseInfoType :: String -> Either String (HsType GhcPs)
parseInfoType str = case unP parser pst of
                     POk _ a -> Right a
                     err -> Left ("Unable to parse '" ++ str ++ "'")
 where defE = ES.insert LangExt.MagicHash $ ES.fromList (languageExtensions Nothing)
#if __GLASGOW_HASKELL__ <= 806
       flgs = ParserFlags empty defE mainUnitId minBound
#else
       flgs = mkParserFlags' empty defE mainUnitId False False False False
#endif
       pst = mkPStatePure flgs (stringToStringBuffer str) loc
       loc = mkRealSrcLoc nilFS 0 0
       parser = unLoc <$> parseType
#endif
