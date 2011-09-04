
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

divsum n = (+(-n)) . product . map (sum . scanl (*) 1 ) . group . factors $ n

limit = 1000000

chain n = go [n]
  where
    go lst@ ~(x:_)
        | x' < n || x' > limit = []
        | x' == n              = lst
        | x' `elem` lst        = []
        | otherwise            = go (x':lst)
      where
        x' = divsum x

problem_95 = minimum $ maximumBy (comparing length) [lst | x <- [2..limit], let lst = chain x, not (null lst)]

main = print problem_95
