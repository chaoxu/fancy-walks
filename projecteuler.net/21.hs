
import Data.List hiding (union)
import Data.List.Ordered (minus,union)
 
primes = 2: 3: [5,7..] `minus` foldr union' []
			   [ [p*p,p*p+2*p..] | p<- tail primes ]
   where union' (q:qs) xs = q : union qs xs

pfactors n toTest@(x:xs) 
	| n <= 1 = []
	| n `mod` x == 0 = x:pfactors (n `div` x) toTest
	| otherwise = pfactors n xs

sumDivisorsFromF = product . map helper . group . sort where
	helper [] = 1
	helper (x:xs) = (helper xs) * x + 1

sumDivisors n = sumDivisorsFromF $ pfactors n primes

sumPdivisors n = (sumDivisors n) - n

check n = m /= n && n == sumPdivisors m
	where m = sumPdivisors n

problem_21 = sum $ filter check [2..10000]

main = print problem_21
