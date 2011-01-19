
import Data.Array


solve n = go False 1 1
  where
    memo_range = ((1,1),(n,n))
    go :: Bool -> Int -> Int -> Integer
    go _ sum num | not $ inRange memo_range (sum,num) = 0
    go _ sum _ | sum == n= 1
    go True sum num = memo ! (sum,num)
    go False sum num = go True (sum+1) (num+1) + go True (sum+num) num
    memo = array memo_range [(i,go False (fst i) (snd i)) | i <- range memo_range]

problem_76 = solve 100 - 1

main = print problem_76
