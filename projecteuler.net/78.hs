
import Data.Array

-- http://oeis.org/A000041 http://oeis.org/A001318

pentagonal = map (\n -> n * (3*n-1) `div` 2) $ concatMap (\n -> [n,-n]) [1..]

partitions :: Int -> Array Int Integer
partitions limit = arr
  where
    arr = array (0,limit) $ (0,1) : [ (n, solve n) | n <- [1..limit]]
    solve n = sum [ s * arr ! (n-d) | (d,s) <- zip (takeWhile (<=n) pentagonal) (cycle [1,1,-1,-1])]

problem_78 = try 1
  where
    try n = if null lst then try (n * 2) else head lst
      where 
        arr = partitions n
        lst = filter (\x -> (arr ! x) `mod` 1000000 == 0) [1..n]

main = print problem_78
