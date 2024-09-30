module Main where

import Ex

main :: IO ()
main = do mapM_ (print . divs) [4, 10,100,128,819,693]
          print $ smallestDiv 4
