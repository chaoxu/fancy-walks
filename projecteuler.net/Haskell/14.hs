{-# OPTIONS_GHC -O2 #-}

import Data.Array
import Data.List

memo limit = arr where 
	arr = listArray (1, limit) $ 0:[go x | x <- [2..limit]]
	go x = if x' <= limit then 1 + arr ! x' else 1 + go x' where
		x' = if even x then x `div` 2 else 3 * x + 1

problem_14 = fst $ maximumBy (\x y -> compare (snd x) (snd y)) $ assocs $ memo 1000000

--main = print problem_14
