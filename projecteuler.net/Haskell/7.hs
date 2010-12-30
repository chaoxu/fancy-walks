
import Data.List.Ordered (minus,union)
 
primes = 2: 3: [5,7..] `minus` foldr union' [] 
		[ [p*p,p*p+2*p..] | p <- tail primes ]
	where union' (q:qs) xs = q : union qs xs


problem_7 = primes !! 10000
