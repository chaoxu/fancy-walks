
import Text.Printf

-- return the probability that needs exactly n steps
getProbability :: Integer -> Double
getProbability n = solve n - solve (n - 1)
  where
    solve n = (1 - 0.5 ^ n) ^ 32

problem_323 = sum [fromIntegral n * getProbability n | n <- [1 .. 1000]]

main = printf "%.10f" problem_323
