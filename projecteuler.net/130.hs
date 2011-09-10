
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

solve n = go 1 1
  where
    go pos v | v == 0    = pos
             | otherwise = go (pos + 1) ((v * 10 + 1) `mod` n)

lst = [n | n <- [2..] `minus` primes, n `mod` 5 /= 0 && odd n, (n - 1) `mod` solve n == 0]

problem_130 = sum $ take 25 lst

main = print problem_130
