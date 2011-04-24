{-# OPTIONS_GHC -O2 #-}

import Data.List hiding (union)
import Data.List.Ordered (minus,union)
import Data.Int
import Data.Maybe
import Data.Ord

primes :: [Int]
primes = 2: 3: [5,7..] `minus` foldr union' []
    [ [p*p,p*p+2*p..] | p <- tail primes ]
  where 
    union' (q:qs) xs = q : union qs xs

isPrime :: Int -> Bool
isPrime = test primes
  where
    test _ n | n <= 1 = False
    test (x:xs) n 
        | x * x > n = True
        | mod n x == 0 = False
        | otherwise = test xs n

solve n = fromMaybe (solve (n * 2)) (try n)

con :: Int -> Int -> Int
con a b = read $ shows b $ show a

check :: Int -> Int -> Bool
check a b = isPrime (con a b) && isPrime (con b a)

try n = listToMaybe $ sortBy (comparing sum)
      [ [a,b,c,d,e]
      | let pa = takeWhile (<=n) primes
      , a <- pa
      , let pb = filter (check a) $ takeWhile (<a) pa
      , b <- pb
      , let pc = filter (check b) $ takeWhile (<b) pb
      , c <- pc
      , let pd = filter (check c) $ takeWhile (<c) pc
      , d <- pd
      , let pe = filter (check d) $ takeWhile (<d) pd
      , e <- pe
      ]

problem_60 = sum $ solve 1000

main = print problem_60
