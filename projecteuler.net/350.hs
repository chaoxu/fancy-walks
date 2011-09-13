
import Test.QuickCheck
import Control.Monad


import Data.List hiding (union)
import Data.Int
import Data.Ord

import Debug.Trace

minus (x:xs) (y:ys) = case (compare x y) of 
          LT -> x : minus  xs  (y:ys)
          EQ ->     minus  xs     ys 
          GT ->     minus (x:xs)  ys
minus  xs     _     = xs

union (x:xs) (y:ys) = case (compare x y) of 
          LT -> x : union  xs  (y:ys)
          EQ -> x : union  xs     ys 
          GT -> y : union (x:xs)  ys
union  xs     ys    = xs ++ ys

primes = 2 : primes'
  where
    primes' = 3 : ([5,7..] `minus` join [[p*p,p*p+2*p..] | p <- primes'])   
    join  ((x:xs):t)    = x : union xs (join (pairs t))
    pairs ((x:xs):ys:t) = (x : union xs ys) : pairs t

factors n = go n primes
  where
    go 1 _ = []
    go n (x:xs)
        | x * x > n      = [n]
        | n `mod` x == 0 = x : go (n `div` x) (x:xs)
        | otherwise      = go n xs


-- return interval [i | n `div` i == d]
getInterval n d = (n `div` (d + 1) + 1, n `div` d)

prop_getInterval (Positive n) = forAll (choose (1, n)) $ \i -> 
    let (l, r) = getInterval n (n `div` i) 
    in 
        and [ n `div` l == n `div` i
            , n `div` r == n `div` i
            , l == 1 || n `div` (l - 1) /= n `div` i
            , n `div` (r + 1) /= n `div` i
            ]

-- return the count of lists with n elements and lcm value m and gcd value 1
solve n m = product [ fromIntegral (e + 1) ^ n - 2 * fromIntegral e ^ n + fromIntegral (e - 1) ^ n :: ModP
                    | xs <- group (factors m)
                    , let e = length xs
                    ]

prop_solve (Positive n) = n > 4 .||. forAll (choose (1, 50 :: Integer)) check
  where
    check m = solve n m == fromInteger (genericLength [ xs 
                                                      | xs <- replicateM n $ filter (\x -> m `mod` x == 0) [1..m]
                                                      , foldl lcm 1 xs == m
                                                      , foldl gcd 0 xs == 1
                                                      ])

newtype ModP = ModP { unModP :: Int64 } deriving Eq

modulo :: Integral a => a
modulo = 101^4

instance Show ModP where
    show (ModP a) = show a

instance Num ModP where
    ModP a + ModP b = ModP $ (a + b) `mod` modulo
    ModP a - ModP b = ModP $ (a - b) `mod` modulo
    ModP a * ModP b = ModP $ (a * b) `mod` modulo
    abs = undefined
    signum = undefined
    fromInteger = ModP . fromInteger . (`mod` modulo)

solveF lo hi n = sum [fromInteger (min hi r - lo + 1) * solve n i | i <- [1..hi `div` lo + 10], let (l, r) = getInterval hi i, min hi r >= lo]

problem_350 = solveF (10^6) (10^12) (10^18)

main = print problem_350
