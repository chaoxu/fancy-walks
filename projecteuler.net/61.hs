{-# OPTIONS_GHC -O2 #-}

import Data.List

list = [[x | x <- takeWhile (<10000) $ scanl (+) 1 [n-1,2*n-3..], x `div` 100 >= 10, x `mod` 100 >= 10] | n <- [3..8]]
list' n = list !! (n - 3)

problem_61 = sum . head $ concatMap solve $ permutations [4..8]

solve xs = concat [map (p:) $ search (div p 100) (mod p 100) xs | p <- list' 3]

search h t [] = if h == t then [[]] else []
search h t (x:xs) = concat [map (p:) $ search h (mod p 100) xs | p <- list' x, div p 100 == t]

main = print $ problem_61
