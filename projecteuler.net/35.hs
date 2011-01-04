
import Data.List hiding (union)
import Data.List.Ordered (minus,union)

import Data.Array
 
primes = 2: 3: [5,7..] `minus` foldr union' []
			   [ [p*p,p*p+2*p..] | p <- tail primes ]
   where union' (q:qs) xs = q : union qs xs

limit = 1000000

primesArr = array (0,limit) [(i, False) | i <- [0..limit]] // 
			zip (takeWhile (<=limit) primes) (cycle [True])

isPrime n = primesArr ! n

rotateRep maxv x = mod x10 maxv + div x10 maxv where x10 = x * 10

checkList n = (n:) . takeWhile (/=n) . tail $ iterate (rotateRep $ 10 ^ len) n
	where len = length $ show n

check a = all isPrime $ checkList a

answer = filter check [1..limit]

problem_35 = length answer

--main = print problem_35
