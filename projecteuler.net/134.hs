
import Math.Sieve.ONeill

powMod a 0 m = 1
powMod a p m | odd p     = powMod a (p - 1) m * a `mod` m
             | otherwise = let b = powMod a (p `div` 2) m in b * b `mod` m

recipMod 0 p = error "recipMod 0"
recipMod i p = powMod i (p - 2) p

-- suffix + 10^n * prefix == 0 (modulo mod)
-- prefix = -suffix * recip (10^n)

solve p1 p2 = ((-p1) * recipMod (10^n) p2 `mod` p2) * 10^n + p1
  where
    n = length $ show p1

problem_134 = sum [solve p1 p2 | (p1, p2) <- zip (takeWhile (<=10^6) primes) (tail primes), p1 >= 5]

main = print problem_134
