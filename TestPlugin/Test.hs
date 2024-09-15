{-# OPTIONS_GHC -fplugin Test.Tasty.Ingredients.Spectrum.Plugin -fhpc #-}

module Main where

f :: Int -> Int
f = (+) 1

main :: IO ()
main = print "testing the module!"
