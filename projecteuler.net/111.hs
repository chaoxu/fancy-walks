
import Data.List hiding (union)
import Data.Ord
import Control.Monad
import Data.Function

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

isPrime n = go n primes
  where
    go n (x:xs)
        | x * x > n      = True
        | n `mod` x == 0 = False
        | otherwise      = go n xs

solve n d = head $ filter (not.null) $ map (filter isPrime.concatMap snd) $ groupBy ((==) `on` fst) $ reverse $ sortBy (comparing fst) $ generateNums n d

generateNums n d = [ (size, map (foldl (\x y -> x * 10 + y) 0) $ sequence subset')
                   | subset <- replicateM n [delete d [0..9], [d]]
                   , let size = length $ filter ((==1).length) subset
                   , let subset' = (delete 0 $ head subset) : tail subset
                   ]

problem_111 = sum $ map (sum.solve 10) [0..9]

main = print $ problem_111
