
import Data.Int
import Debug.Trace

limit = 10 ^ 9 :: Int64

sqs = map (^2) [0..]

problem_94 = go 2 0 sqs sqs :: Int64
  where
    go a sum lsta lstb
        | p * 2 > limit = sum
        | head lst == v = go (a + 1) (sum + p * 2) lsta' lstb'
        | otherwise     = go (a + 1) sum lsta' lstb'
      where
        b = a + 1
        p = if even a then a `div` 2 + b else a + b `div` 2
        v = p * (p - if even a then a else b)
        (lsta', lstb')
            | even a    = (dropWhile (<v) lsta, lstb)
            | otherwise = (lsta, dropWhile (<v) lstb)
        lst = if even a then lsta' else lstb'

main = print problem_94
