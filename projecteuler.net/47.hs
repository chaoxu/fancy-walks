
import Data.List hiding (union)
import Data.List.Ordered (minus,union)
 
primes = 2: 3: [5,7..] `minus` foldr union' [] [[p*p,p*p+2*p..] | p <- tail primes]
  where 
    union' (q:qs) xs = q : union qs xs

factors n toTest@(x:xs) 
    | n <= 1 = []
    | n < x*x = [n]
    | n `mod` x == 0 = addHead x $ factors (n `div` x) toTest
    | otherwise = factors n xs
  where
    addHead u [] = [u]
    addHead u v@(x:xs) = if u == x then v else u:v

check m n = all (\x -> (==m).length $ factors x primes) [n,n+1..n+m-1]

answer m = head $ filter (check m) [1..]

problem_47 = answer 4

main = print problem_47
