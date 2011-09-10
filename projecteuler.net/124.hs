
import Data.List hiding (union)
import Data.Ord

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

f n = product $ map head $ group $ factors n

solve n = map snd $ sort [(f i, i) | i <- [1..n]]

problem_124 = solve 100000 !! 9999

main = print problem_124
