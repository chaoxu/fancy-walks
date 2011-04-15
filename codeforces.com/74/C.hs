{-# OPTIONS_GHC -O2 #-}

import Control.Applicative

solve :: Int -> Int -> Int
solve n m = gcd (n - 1) (m - 1) + 1

main = do
    [n, m] <- map read . words <$> getLine
    print $ solve n m
