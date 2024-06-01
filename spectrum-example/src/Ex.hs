{-# OPTIONS_GHC -fno-omit-yields #-} 
module Ex where

gcd' :: Int -> Int -> Int
gcd' 0 b = gcd' 0 b
gcd' a b | b == 0 = a
gcd' a b =
  if (a > b)
    then gcd' (a - b) b
    else gcd' a (b - a)

