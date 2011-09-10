
import Data.List hiding (union)
import Data.Ord
import qualified Data.IntSet as Set
import Data.Array

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
    go n (x:xs)
        | x * x > n      = [n]
        | n `mod` x == 0 = x : go (n `div` x) (x:xs)
        | otherwise      = go n xs

rad n = product $ map head $ group $ factors n

solve n = sum [ c
              | (c, rc) <- lst
              , (a, ra) <- takeWhile (\(_,r) -> r * rc < c) lst
              , let b = c - a
              , a < b
              , gcd a c == 1
              , ra * rads ! b * rc < c
              ]
  where
    rads = listArray (1, n) $ map rad [1..]
    
    lst = sortBy (comparing snd) $ assocs rads

problem_127 = solve (120000 - 1)

main = print problem_127
