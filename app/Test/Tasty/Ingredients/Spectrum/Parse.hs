{-# LANGUAGE CPP #-}
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


parseInfoType :: String -> Maybe (HsType GhcPs)
parseInfoType str = case unP parser pst of
                        POk _ a -> Just a
                        PFailed {} -> Nothing
  where

#if __GLASGOW_HASKELL__ >= 904

#if __GLASGOW_HASKELL__ < 908
        emptyDiagOpts = DiagOpts empty empty False False Nothing defaultSDocContext
#endif
        flgs = mkParserOpts empty emptyDiagOpts [] False False False False
        pst = initParserState flgs (stringToStringBuffer str) loc
#elif __GLASGOW_HASKELL__ == 902
        flgs = mkParserOpts empty empty False False False False
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

parseInfoType :: String -> Maybe (HsType GhcPs)
parseInfoType str = case unP parser pst of
                     POk _ a -> Just a
                     PFailed {} -> Nothing
 where
#if __GLASGOW_HASKELL__ <= 806
       flgs = ParserFlags empty empty mainUnitId minBound
#else
       flgs = mkParserFlags' empty empty mainUnitId False False False False
#endif
       pst = mkPStatePure flgs (stringToStringBuffer str) loc
       loc = mkRealSrcLoc nilFS 0 0
       parser = unLoc <$> parseType
#endif
