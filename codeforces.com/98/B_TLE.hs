{-# OPTIONS_GHC -O2 #-}

import Data.Ratio

main = interact $ show' . solve . read
  where
    show' a = show (numerator a) ++ "/" ++ show (denominator a)

solve :: Int -> Rational
solve 1 = 0
solve n | even n = solve (n `div` 2) + 1

-- let f(x) be number of leaves to expend, f(1) is the desired result
-- let x' = (x * 2) mod n
-- then there are x' / (x * 2) probability lead to f(x') 
solve n = solveInfiniteRecursive a b next 1
  where
    a x = fromIntegral (next x) / fromIntegral (2 * x)
    b x = 1
    next x = x * 2 `mod` n


data RecursiveResult i a = Independence a
                         | Dependence i a a

-- solve equations f(x) = a(x) * f(next(x)) + b(x)
solveInfiniteRecursive :: (Eq i, Fractional a) => (i -> a) -> (i -> a) -> (i -> i) -> i -> a
solveInfiniteRecursive a b next x = case go x x of Independence val -> val
  where
    go x y 
        | x' == y'' = mergeResult y . mergeResult y' $ Dependence x' 1 0
        | otherwise = mergeResult y . mergeResult y' $ go x' y''
      where
        x' = next x
        y' = next y
        y'' = next y'

    -- f(x) = val * a + b
    mergeResult x (Independence val) = Independence (val * a x + b x)

    -- f(x) = (f(y) * a' + b') * a + b
    -- f(x) = f(y) * a'' + b''
    mergeResult x (Dependence y a' b')
        | x == y    = Independence (b'' / (1 - a''))
        | otherwise = Dependence y a'' b''
      where
        ax = a x
        a'' = a' * ax
        b'' = b' * ax + b x
