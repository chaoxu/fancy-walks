
import Data.List

limit = 1500000

-- http://en.wikipedia.org/wiki/Pythagorean_triple
pythTriples = concat
              [ takeWhile (<=limit) [sum,sum*2..]
              | m <- [1..1000]
              , n <- [1..m-1]
              , gcd m n == 1 && (even m || even n)
              , let a = m^2 - n^2
              , let b = 2*m*n
              , let c = m^2 + n^2
              , let sum = a + b + c
              , sum <= limit
              ]

problem_75 = length $ filter ((==1).length) $ group $ sort pythTriples

main = print problem_75
