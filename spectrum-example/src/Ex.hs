module Ex where

-- Taken from Tarantula
mid :: Int -> Int -> Int -> Int
mid x y z =
  if y < z
  then if x < y then y
       else if x < z
            then y -- should be x
            else z
  else if x > y then y
       else if x > z
            then x
            else z

