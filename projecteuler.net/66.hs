
import Data.List
import Data.Maybe
import Data.Ord

isqrt :: Integer -> Integer
isqrt n = fst . head $ dropWhile (\(a,b) -> a + 1 /= b && a /= b) $ iterate (\(_,x) -> (x, (x + div n x) `div` 2)) (-1, 1)

next :: Integer -> (Integer,(Integer,Integer)) -> (Integer,(Integer,Integer))
next n (_,(a,b)) = (z,(a',b'))
  where
    sqrtn = isqrt n
    z = quot (sqrtn + a) b
    a' = -(a - z * b)
    b' = div (n-a'^2) b

cf :: Integer -> [Integer]
cf n = tail $ map fst $ iterate (next n) (0,(0,1))

cfCalc :: [Integer] -> (Integer,Integer)
cfCalc = foldr (\n (x,y) -> (y+x*n,x)) (1,0) 

solveDioph :: Integer -> (Integer,Integer)
solveDioph n = head $ dropWhile (\(x,y) -> x^2 - n * y^2 /= 1) $ map cfCalc $ tail $ inits $ cf n

isSquare n = sqrtn^2 == n
  where
    sqrtn = isqrt n

problem_66 = fst $ maximumBy (comparing (fst.snd)) list
  where
    list = map (\x -> (x, solveDioph x)) $ filter (not.isSquare) [2..1000]

main = print problem_66
