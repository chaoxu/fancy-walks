{-# OPTIONS_GHC -O2 #-}

import Data.List
import Control.Monad

-- http://mathworld.wolfram.com/IrreduciblePolynomial.html

sieve (x:xs) = x : filter ((/=0).(`mod` x)) xs
primes = sieve [2..]

pfactors n toTest@(x:xs) 
    | n <= 1 = []
    | x * x > n = [n]
    | n `mod` x == 0 = x:pfactors (n `div` x) toTest
    | otherwise = pfactors n xs

mobius :: Integer -> Integer
mobius 1 = 1
mobius n = if fs == fs' then (-1)^(length fs) else 0
  where
    fs = pfactors n primes
    fs' = nub fs

divisors :: Integer -> [Integer]
divisors n = sort $ map product $ sequence mul
  where
    fs = group $ pfactors n primes
    mul = map (scanl (*) 1) fs

count :: Integer -> Integer -> Integer
count q n = (`div` n) $ sum $ map (\d -> mobius (n `div` d) * q^d) $ divisors n

main = interact $ unlines . map (show . count 2 . read) . lines

