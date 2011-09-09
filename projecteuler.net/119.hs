{-# OPTIONS_GHC -O2 #-}

import Data.List
--import Data.Int

minusOrd :: Ord a => [a] -> [a] -> [a]
minusOrd a@(x:xs) b@(y:ys) = case compare x y of
    EQ -> minusOrd xs ys
    LT -> x : minusOrd xs b
    GT -> minusOrd a ys
minusOrd xs _ = xs

unionOrd :: Ord a => [a] -> [a] -> [a]
unionOrd a@(x:xs) b@(y:ys) = case compare x y of
    EQ -> x : unionOrd xs ys
    LT -> x : unionOrd xs b
    GT -> y : unionOrd a ys
unionOrd xs ys = xs ++ ys

pairsOrd :: Ord a => [[a]] -> [[a]]
pairsOrd [] = []
pairsOrd [xs] = [xs]
pairsOrd ((x:xs):ys:remain) = (x : unionOrd xs ys) : pairsOrd remain

joinOrd :: Ord a => [[a]] -> [a]
joinOrd [] = []
joinOrd [xs] = xs
joinOrd ((x:xs):remain) = x : unionOrd xs (joinOrd (pairsOrd remain))

primes = sieve [2..]
  where
    sieve (x:xs) = x : sieve (filter ((/=0).(`mod` x)) xs)

powerNumber = dropWhile (<10) $ joinOrd [map (^i) [2..] | i <- primes]

digitSum 0 = 0
digitSum x = let (a, b) = x `divMod` 10 in digitSum a + b

check n
    | d == 1    = False
    | otherwise = n `mod` d == 0 && go (d^2)
  where
    d = digitSum n

    go x | x > n     = False
         | x == n    = True
         | otherwise = go (x * d)

interestings = filter check powerNumber

problem_119 = interestings !! 29

main = print problem_119
