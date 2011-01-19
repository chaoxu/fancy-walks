{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Array.Unboxed

primes = sieve [2..]
  where
    sieve (x:xs) = x : sieve (filter ((/=0).(`mod` x)) xs)

limit = 50000000

ans = [ a + b + c
      | a <- l4
      , b <- takeWhile (<limit - a) l3
      , c <- takeWhile (<limit - a - b) l2
      ]
  where
    list n = takeWhile (<limit) $ map (^n) primes
    l2 = list 2
    l3 = list 3
    l4 = list 4

bitmap :: Array Int Bool
bitmap = accumArray (||) False (1,limit) $ map (\x -> (x,True)) ans

problem_87 = length $ filter (bitmap!) [1..limit]

main = print problem_87
