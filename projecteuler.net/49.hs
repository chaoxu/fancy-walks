
import Data.List

sieve (x:xs) = x : sieve (filter ((/=0).(`mod` x)) xs)
sieve [] = []

primes = sieve [2..]

isPrime = test primes
  where
    test _ n | n <= 1 = False
    test (x:xs) n 
        | x * x > n = True
        | mod n x == 0 = False
        | otherwise = test xs n

targets = takeWhile(<10000) $ dropWhile (<1000) primes

answers = [show a ++ show c ++ show b | a <- targets, b <- targets, a < b, let c = (a + b) `div` 2, isPrime c, dig a == dig b && dig a == dig c]
  where
    dig = sort . show

problem_49 = filter (/="148748178147") answers

main = putStrLn $ head problem_49
