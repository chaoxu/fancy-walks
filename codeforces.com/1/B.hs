{-# OPTIONS_GHC -O2 #-}

import Data.Char

to26 :: Int -> [Char]
to26 n = helper n 26
  where
    helper n k | k >= n = to26' (n-1) k
    helper n k = helper (n - k) (k * 26)
    to26' n k
        | k == 1 = []
        | otherwise = chr (ord 'A' + div') : to26' mod' k'
          where
            k' = k `div` 26
            (div', mod') = divMod n k'

from26 :: [Char] -> Int
from26 s = from26' (reverse s) + 1 + sum [ 26^i | i <- [1..len-1]]
  where
    len = length s
    from26' [] = 0
    from26' (x:xs) = from26' xs * 26 + (ord x - ord 'A')

checkRC :: [Char] -> Bool
checkRC str = (head str == 'R') && (isDigit $ str !! 1) && (elem 'C' str)

rc2xy :: [Char] -> [Char]
rc2xy str = to26 c ++ show r
  where
    [r,c] = map read . words $ map (\ch -> if isDigit ch then ch else ' ') str

xy2rc :: [Char] -> [Char]
xy2rc str = "R" ++ y ++ "C" ++ show (from26 x)
  where
    (x,y) = span isUpper str

solve :: [Char] -> [Char]
solve str = if checkRC str then rc2xy str else xy2rc str

main = interact $ unlines . (\(n:b) -> map solve $ take (read n) b) . lines
