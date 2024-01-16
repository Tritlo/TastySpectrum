module Test.Tasty.Ingredients.Spectrum.Weights where 


import Trace.Hpc.Util

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Debug.Trace

data ModuleResult = Result FilePath
                       [(HpcPos, [Double])] 
    deriving (Show)

weightsParse :: FilePath -> IO ([Text], -- Rules
                         [ModuleResult])
weightsParse fp = do (_:file) <- T.lines <$> T.readFile fp
                     let (rule_txts, (_:_:rest)) = span (not . T.null) file
                         rules = map (T.drop 2) rule_txts
                         modResChunking [] = []
                         modResChunking (a:b:r) = (a,b):(modResChunking r)
                         mod_res_chunks = modResChunking rest
                         parseChunk (fp_text,ws_text) = Result (T.unpack fp)
                                                            $ parseWs ws
                          where fp = T.drop 2 $ T.dropEnd 1 fp_text
                                ws = T.drop 4 $ ws_text
                         parseWs ws_t = parseWs' ws
                             where
                               ws = T.drop 1 $ T.dropEnd 1 $ ws_t
                               parseWs' ws | (w,r) <- T.span (/= ')') ws,
                                             r' <- T.drop 2 $ r,
                                             w' <- parseW (T.drop 1 w)
                                   = if T.null r' then  [w']
                                     else w':(parseWs' r')
                               parseW w | (hp, r) <- T.span (/= ',') w,
                                          r' <- T.drop 2 $ T.dropEnd 1 r,
                                          hp' <- T.drop 1 $ T.dropEnd 1 hp,
                                          rs <- T.splitOn (T.pack ",") r'
                                     = (read $ T.unpack hp',
                                        map (read . T.unpack) rs)
                                          


                                        
                                                
                                        
                                                  
                                                    
                                

                     print rules
                     print $ length mod_res_chunks
                     print $ map parseChunk mod_res_chunks
                     error "ok"
              
