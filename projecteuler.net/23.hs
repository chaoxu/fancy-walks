
import Data.List hiding (union)
import Data.List.Ordered (minus,union)
import Data.Array
 
primes = 2: 3: [5,7..] `minus` foldr union' []
			   [ [p*p,p*p+2*p..] | p<- tail primes ]
   where union' (q:qs) xs = q : union qs xs

pfactors n toTest@(x:xs) 
	| n <= 1 = []
	| n `mod` x == 0 = x:pfactors (n `div` x) toTest
	| otherwise = pfactors n xs

sumDivisorsFromF = product . map helper . group . sort where
	helper [] = 1
	helper (x:xs) = helper xs * x + 1

sumDivisors n = sumDivisorsFromF $ pfactors n primes

sumPdivisors n = sumDivisors n - n

isAbundant n = sumPdivisors n > n

limit = 28123

lst = filter isAbundant [1..limit]

arr = listArray (1, limit) $ map isAbundant [1..limit]

isSum n = any (arr!) [n - x | x <- lst , x + x <= n]

problem_23 = sum . filter (not . isSum) $ [1..limit]

main = print problem_23
