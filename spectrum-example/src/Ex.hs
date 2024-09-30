module Ex where

divs :: Int -> [Int]
divs n = go 2
  where
    go i | i == n = []
    go i = if d i
            then i : go (i + 1)
            else go (i + 1)
    d i = n `mod` i == 0

smallestDiv n = head (divs n)
