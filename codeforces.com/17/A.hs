
import Control.Applicative
import Data.Array

primes = sieve [2..]
  where
    sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

solve :: Int -> Int -> String
solve n k = if length answers >= k then "YES" else "NO"
  where
    prim = takeWhile (<=n) primes
    arr = accumArray (||) False (2,n) [(p, True) | p <- prim]
    isPrime n = arr ! n
    answers = [x + y + 1 | (x, y) <- zip prim (tail prim), x + y + 1 <= n, isPrime (x + y + 1)]

main = do
    [n,k] <- map read.words <$> getLine
    putStrLn $ solve n k

