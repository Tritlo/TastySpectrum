{-# LANGUAGE RecordWildCards #-}
module Test.Tasty.Ingredients.Spectrum.Types (
        Label (..),
        TestResults,
        module Trace.Hpc.Util
            ) where


import Trace.Hpc.Util 

import Data.Function (on)

data Label = Label {loc_name :: String,
                    loc_pos :: HpcPos,
                    loc_index :: Int,
                    loc_evals :: [(Int,Integer)]}


instance Show Label where
    show (Label {..}) = 
        loc_name ++ ":" ++ show loc_pos ++ " " ++ show (loc_evals)

instance Eq Label where
  (==) = (==) `on` loc_index

instance Ord Label where
  compare = compare `on` loc_index


type TestResults = ([(String,Bool)], [Label])
