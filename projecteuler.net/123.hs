
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

powMod a 0 c = 1 `mod` c
powMod a b c | even b    = let d = powMod a (b `div` 2) c in d * d `mod` c
             | otherwise = powMod a (b - 1) c * a `mod` c

limit = 10^10
solve n (pn:ps)
    | limit >= modulo = solve (n + 1) ps
    | r > limit       = n
    | otherwise       = solve (n + 1) ps
  where
    modulo = pn ^ 2
    r = (powMod (pn - 1) n modulo + powMod (pn + 1) n modulo) `mod` modulo

problem_123 = solve 1 primes

main = print problem_123
