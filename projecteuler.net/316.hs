{-# OPTIONS_GHC -O2 #-}

import Data.Char

expect k = foldl (\x y -> if y then x * 10 + 1 else x * 10) 0 bits where
	s = show k
	helper [] [] = [True]
	helper x@(_:nx) y@(_:ny) = (x == reverse y) : helper nx ny
	bits = helper s (reverse s)

g n = (expect n) - (toInteger . length $ show n)

problem_316 = sum $ map (g.div (10^16)) [2..999999]

main = print problem_316
