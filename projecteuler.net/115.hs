
import Data.Array

solve m = (cache!)
  where
    bnds = (0, 10000)
    cache = listArray bnds $ map go $ range bnds

    go 0 = 1
    go n = sum lst + go (n - 1)
      where
        lst = [if i == n then 1 else solve m (n - 1 - i)  | i <- [m..n]]

problem_115 = head [i | i <- [1..], solve 50 i > 10^6]

main = print problem_115
