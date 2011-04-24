
import Data.List hiding (union)
import Data.List.Ordered (minus,union)
 
primes = 2: 3: [5,7..] `minus` foldr union' []
			   [ [p*p,p*p+2*p..] | p<- tail primes ]
   where union' (q:qs) xs = q : union qs xs

pfactors n toTest@(x:xs) 
	| n <= 1 = []
	| n `mod` x == 0 = x:pfactors (n `div` x) toTest
	| otherwise = pfactors n xs

divisorsFromF = product . map ((+1).length) . group . sort

divisors n = divisorsFromF $ pfactors n primes

divisors2 x y = divisorsFromF $ pfactors x primes ++ pfactors y primes

divisors2' n 
	| even n = divisors2 (n `div` 2) (n+1)
	| odd n  = divisors2 n ((n+1) `div` 2)

problem_12 = (\n -> n*(n+1) `div` 2) $ head $ dropWhile ((<=500).divisors2') [1..]

main = print problem_12
