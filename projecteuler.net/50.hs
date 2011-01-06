{-# OPTIONS_GHC -O2 #-}

import Control.Monad
import Data.List hiding (union)
import Data.List.Ordered (minus,union)

import Data.Array
import Data.Maybe
 
primes = 2: 3: [5,7..] `minus` foldr union' []
    [ [p*p,p*p+2*p..] | p <- tail primes ]
  where 
    union' (q:qs) xs = q : union qs xs

isPrime = test primes
  where
    test _ n | n <= 1 = False
    test (x:xs) n 
        | x * x > n = True
        | mod n x == 0 = False
        | otherwise = test xs n

limit = 1000000

targets = takeWhile (<= div limit 21) primes

solve ps tlen
    | len < tlen = Nothing
    | minsum > limit = Nothing
    | isPrime minsum = Just $ take tlen ps
    | otherwise = solve (tail ps) tlen
  where
    len = length ps
    minsum = sum $ take tlen ps

mysolve ps = foldl mplus Nothing $ map (solve ps) $ [lps,lps-1..1]
  where
    lps = length ps

problem_50 = sum . fromJust $ mysolve targets

main = print problem_50
