
import Data.List hiding (union)
import Data.List.Ordered (minus,union)

import Data.Array
 
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

perm :: [a] -> [[a]]
perm [] = [[]]
perm (x:xs) = concatMap (insertAll x []) $ perm xs
  where
    insertAll u prev [] = [prev ++ [u]]
    insertAll u prev t@(v:vs) = (prev ++ u:t) : insertAll u (prev ++ [v]) vs

arrayToInt = foldl (\x y -> x * 10 + y) 0

checkList = map arrayToInt $ concatMap (\n -> perm [1..n]) [1..9]

problem_41 = maximum $ filter isPrime checkList

--main = print problem_41
