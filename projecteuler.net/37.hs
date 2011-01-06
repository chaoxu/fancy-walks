
import Data.List hiding (union)
import Data.List.Ordered (minus,union)

import Data.Array
 
primes = 2: 3: [5,7..] `minus` foldr union' []
			   [ [p*p,p*p+2*p..] | p <- tail primes ]
	where union' (q:qs) xs = q : union qs xs

isPrime = test primes
	where
		test _ n | n <= 1 = False
		test (x:xs) n 
			| x * x > n = True
			| mod n x == 0 = False
			| otherwise = test xs n

isPrime' n = isPrime n && (n < 10 || isPrime' (read.tail$show n))

lst0 = [2,3,5,7]
next lst = concatMap (\x -> [x' | y <- [1..9], let x' = x * 10 + y, isPrime x']) lst

answer = drop 4 $ filter isPrime' $ concat $ reverse $ solve [lst0]
    where
        solve ([]:xs) = xs
        solve (x:xs) = solve (next x:x:xs)

problem_37 = sum answer

main = print problem_37
