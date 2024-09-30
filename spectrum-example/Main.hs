module Main where

import Ex

main :: IO ()
main = mapM_ (print . divs) [10,100,128,819,693]
