
-- http://oeis.org/A029549

-- x*(x+1) = 2 * y * (y + 1)
-- 4 x^2 + 4 x = 2 (4 y ^ 2 + 4 y)
-- (2x+1)^2 - 1 = 2 (2y+1)^2 - 2
-- (2x+1)^2 - 2 (2y+1)^2 = -1
-- X^2 - 2 Y^2 = -1

import Data.Ratio

continuedFractionSqrt2 = 1 : repeat 2

rationalFromCF seq = drop 2 lst
  where
    lst = (0, 1) : (1, 0) : zipWith3 go lst (tail lst) seq
    go (x1, y1) (x2, y2) a = (x1 + x2 * a, y1 + y2 * a)

problem_100 = snd $ head lst :: Integer
  where
    lst = [ (all, blue)
          | (x, y) <- rationalFromCF continuedFractionSqrt2
          , odd x && odd y
          , x ^ 2 - 2 * y ^ 2 == -1
          , let all = x `div` 2 + 1
          , let blue = y `div` 2 + 1
          , all >= 10^12
          ]

main = print problem_100

