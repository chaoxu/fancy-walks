import Data.List

getFactor n = (\r -> if r == [] then 0 else head r) $ dropWhile (\r -> n `rem` r > 0) $ takeWhile (\r -> r * r <= n) [2..]

primeFactors n =
    if p == 0
    then [n]
    else (primeFactors p) ++ (primeFactors $ n `div` p)
    where p = getFactor n

phi n = (foldl (\z r -> z - z `div` (head r)) n) . group . sort . primeFactors $ n
    
