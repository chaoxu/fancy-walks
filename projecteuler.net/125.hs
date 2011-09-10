{-# OPTIONS_GHC #-}

import Data.Int

import Debug.Trace

sqSums = scanl (+) 0 [i ^ 2 | i <- [1..]] :: [Int64]

findDiff diff = go sqSums sqSums
  where
    go (x:xs) y'
        | head xs == head ys = False
        | d == 0             = False
        | d == diff          = True
        | otherwise          = go xs ys
      where
        ys = dropWhile (<x+diff) y'
        d = head ys - x

palindromes = concatMap construct [1..]
  where

construct n = [go x (if odd n then x `div` 10 else x) | x <- [10^(n'-1)..10^n'-1]]
  where
    n' = (n + 1) `div` 2
    go x 0 = x
    go x y = go (x * 10 + y `mod` 10) (y `div` 10)

problem_125 = sum $ filter findDiff $ takeWhile (<10^8) palindromes

main = print problem_125
