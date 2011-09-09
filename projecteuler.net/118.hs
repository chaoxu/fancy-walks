
import Data.List hiding (union)
import Data.Ord
import qualified Data.Map as Map
import Data.Map ((!))

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

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = go n primes
  where
    go n (x:xs)
        | x * x > n      = True
        | n `mod` x == 0 = False
        | otherwise      = go n xs

primesWithMask subset = [ number
                        | digits <- permutations subset
                        , let number = foldl (\a b -> a * 10 + b) 0 digits
                        , isPrime number
                        ]


primesWithMaskNum = (cache!)
  where
    cache = Map.fromList [(subset, length $ primesWithMask subset) | subset <- subsequences [1..9]]

solve = (cache !)
  where
    cache = Map.fromList [(subset, go subset) | subset <- subsequences [1..9]]

    go [] = 1
    go (x:xs) = sum lst
      where
        lst = [ primesWithMaskNum (x:ss) * solve (xs \\ ss)
              | ss <- subsequences xs
              ]

problem_118 = solve [1..9]

main = print problem_118
