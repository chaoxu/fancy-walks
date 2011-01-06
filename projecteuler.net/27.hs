{-# OPTIONS_GHC -O2 #-}

import Data.List hiding (union)
import Data.List.Ordered (minus,union)
import Data.Ord;
 
primes = 2: 3: [5,7..] `minus` foldr union' []
			   [ [p*p,p*p+2*p..] | p<- tail primes ]
   where union' (q:qs) xs = q : union qs xs

isPrime n (x:xs) 
	| n <= 1 = False
	| x * x > n = True
	| n `mod` x == 0 = False
	| otherwise = isPrime n xs

testPoly a b x = if isPrime val primes then testPoly a b (x + 1) else x where
	val = (x + a) * x + b

range = [-999..999]

problem_27 = fst $ maximumBy (comparing snd) [(a * b, testPoly a b 0) | a <- range, b <- range]

main = print problem_27
