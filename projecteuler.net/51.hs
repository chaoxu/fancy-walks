import Data.List hiding (union)
import Data.List.Ordered (minus,union)

primes = 2: 3: [5,7..] `minus` foldr union' []
    [ [p*p,p*p+2*p..] | p <- tail primes ]
  where 
    union' (q:qs) xs = q : union qs xs

isPrime = test primes
  where
    test _ n | n <= 1 = False
    test (x:xs) n 
        | x * x > n = True
        | mod n x == 0 = False
        | otherwise = test xs n


addDigit _ [] = []
addDigit dig (x:xs) = (x*10+dig):addDigit dig xs

addDigit' _ [] = []
addDigit' dig (x:xs) = (x*10+dig):addDigit' (dig+1) xs

checkList0 = [1,2,3,4,5,6,7,8,9] : map (replicate 10) [1..9]

checkList = checkList0 ++ concatMap (\n -> addDigit' (10 - length n) n : map (`addDigit` n) [0..9]) checkList 

checkNonEqual (x:y:_) = x /= y

check = (>=8) . length . filter isPrime 

problem_51 = head $ map (head . filter isPrime) $ filter check $ filter checkNonEqual checkList

main = print problem_51
