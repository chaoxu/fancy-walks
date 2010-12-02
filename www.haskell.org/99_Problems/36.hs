
import Data.List

getFactor n = (\r -> if r == [] then 0 else head r) $ dropWhile (\r -> n `rem` r > 0) $ takeWhile (\r -> r * r <= n) [2..]

primeFactors n =
    if p == 0
    then [n]
    else (primeFactors p) ++ (primeFactors $ n `div` p)
    where p = getFactor n

prime_factors_mult = map (\r -> (head r, length r)) . group . sort . primeFactors
