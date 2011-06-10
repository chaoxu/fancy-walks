
import Control.Applicative
import Control.Monad

import Data.List
import Data.Maybe
import Data.Ratio

main = do
    cas <- readLn
    replicateM cas $ do
        [i, j, k] <- map read . words <$> getLine
        let res = fmap (/(10^i)) $ solve (j - i) k
        putStrLn $ show' res
  where
    show' Nothing = "NO SOLUTION"
    show' (Just a) = show (numerator a) ++ " " ++ show (denominator a)

solve :: Int -> Int -> Maybe Rational
solve p k = listToMaybe ans
  where
    p10 = 10^p
    p9 = p10 - 1

    ans = [ (p9 * delta) % (p10 * fromIntegral (k-1))
          | delta <- [1..9]
          , let p9delta = p9 * delta `div` fromIntegral (k-1)
          , let digit0 = p9delta `div` p10
          , let digitp = p9delta `mod` 10
          , digitp == digit0 + delta
          ]
