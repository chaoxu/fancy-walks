
import Data.Ratio
import Data.Array

rounds = 15 :: Int

simulate :: (Int, Int) -> Rational
simulate = (cache!)
  where
    bnds = ((0, -rounds), (rounds, rounds))
    cache = listArray bnds $ map go $ range bnds

    go (r, delta) | r == rounds = if delta > 0 then 1 else 0
    go (r, delta) = blue + red
      where
        prob = 1 % toInteger (2 + r)
        blue = simulate (r + 1, delta + 1) * prob
        red = simulate (r + 1, delta - 1) * (1 - prob)
    

problem_121 = fst $ properFraction $ recip prob
  where
    prob = simulate (0, 0)

main = print $ problem_121
