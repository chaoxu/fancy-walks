
import Data.Array

solve = (cache!)
  where
    bnds = (0, 100)
    cache = listArray bnds $ map go $ range bnds

    go 0 = 1
    go n = sum lst + go (n - 1)
      where
        lst = [if i == n then 1 else solve (n - 1 - i)  | i <- [3..n]]

problem_114 = solve 50

main = print problem_114
