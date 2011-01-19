{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe

isqrt :: Integer -> Integer
isqrt n = fst . head $ dropWhile (\(a,b) -> a + 1 /= b && a /= b) $ iterate (\(_,x) -> (x, (x + div n x) `div` 2)) (-1, 1)

isSquare :: Integer -> Bool
isSquare n = n' * n' == n where n' = isqrt n

solve c = sum 
        [ amax - amin + 1
        | aplusb <- [2..2 * c]
        , isSquare (aplusb^2+c^2)
        , let bmax = min (aplusb - 1) c
        , let amin = aplusb - bmax
        , let amax = aplusb `div` 2]

sigma = scanl (+) 0 $ map solve [1..]

problem_86 = fromJust $ findIndex (>10^6) sigma

main = print problem_86
