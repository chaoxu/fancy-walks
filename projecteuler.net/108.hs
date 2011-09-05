{-# OPTIONS_GHC -O2 #-}

import Data.List

-- (x + y) n = xy
-- (x - n)(y - n) = n^2
-- answer is (#divisors of n^2 + 1) `div` 2

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
    sieve (x:xs) = x : sieve (filter ((/=0).(`mod`x)) xs)

optimumNumbers = go 1 primes [1..]
  where
    go mul (p:ps) lst = mul : joinOrd [go (mul * p^k) ps [1..k] | k <- lst]

factors n = go n primes
  where
    go x (p:ps)
        | p * p > x      = [x]
        | x `mod` p == 0 = p : go (x `div` p) (p:ps)
        | otherwise      = go x ps

solve :: Integer -> Integer
solve n = (ds + 1) `div` 2
  where
    ds = product $ map ((+1).(*2).genericLength) $ group $ factors n

limit = 1000
problem_108 = head [ p | p <- optimumNumbers , solve p > limit ]

main = print problem_108
