{-# OPTIONS_GHC -O2 #-}

import Data.Int
import Data.Array
import Data.List hiding (union)
import Data.List.Ordered (minus,union)

primes = 2: 3: [5,7..] `minus` foldr union' []
               [ [p*p,p*p+2*p..] | p<- tail primes ]
  where 
    union' (q:qs) xs = q : union qs xs

multiplier = 1234567890
modulo = 10^18

-- return the exponent of prime p in n!
getPower :: Int64 -> Int64 -> Int64
getPower n p = if ndivp == 0 then 0 else ndivp + getPower ndivp p
  where
    ndivp = n `div` p

-- return the minimum n, s.t. n! is a multiple of p^a
getPowerRev :: Int64 -> Int64 -> Int64
getPowerRev a p = bsearch 0 (a*p)
  where
    bsearch lo hi
        | lo > hi             = error "getPowerRev: out of range"
        | lo == hi            = lo
        | getPower mid p >= a = bsearch lo mid
        | otherwise           = bsearch (mid + 1) hi
      where
        mid = (lo + hi) `div` 2

problem_320 s = foldl (\x y -> (x + y) `mod` modulo) 0 $ map (arr2!) [10..s]
  where
    bnds = (2, s)
    arr = accumArray max 0 bnds inc 
    inc = [ (n, n')
          | p <- takeWhile (<=s) primes
          , n <- takeWhile (<=s) [p,p+p..]
          , let a = getPower n p
          , let a' = a * multiplier
          , let n' = getPowerRev a' p
          ]
    arr2 = listArray bnds [if i == fst bnds then arr!i else max (arr!i) (arr2!(i-1)) | i <- range bnds]

main = print $ problem_320 1000000
