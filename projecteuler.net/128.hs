
import Data.List hiding (union)
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

isPrime n | n < 2 = False
isPrime n = go n primes
  where
    go n (x:xs)
        | x * x > n      = True
        | n `mod` x == 0 = False
        | otherwise      = go n xs

lst = 1 : scanl (+) 2 [i * 6 | i <- [1..]]

findRing n = head [(l, r) | (l, r) <- zip lst (tail lst), l <= n && n < r]

nextRing (l, r) = (r, r + (r - l + 6))
prevRing (l, r) = (l - (r - l - 6), l)

findPrevNext (l, r) x = [prev, next]
  where
    prev = if x == l then r - 1 else x - 1
    next = if x + 1 == r then l else x + 1

surrounding 1 = [2..7]
surrounding x
    | pos == 0  = sort $ [inner, outer] ++ findPrevNext (nextRing ring) outer ++ findPrevNext ring x
    | otherwise = sort $ findPrevNext ring x ++ [inner', inner'', outer + pos, outer + pos + 1]
  where
    ring = (l, r)
    (l, r) = findRing x
    steps = (r - l) `div` 6

    (edge, pos) = (x - l) `divMod` steps
    
    outer = r + (steps + 1) * edge
    inner = if steps == 1 then 1 else l - (r - l - 6) + (steps - 1) * edge

    inner' = inner + pos - 1
    inner'' = findPrevNext (prevRing ring) inner' !! 1

pd n = length [x | x <- surrounding n, isPrime (abs (n - x))]

potential = tail $ concat [[l, r - 1] | (l, r) <- zip lst (tail lst)] 

answers = [i | i <- potential, pd i == 3]

problem_128 = answers !! 1999

main = print problem_128
