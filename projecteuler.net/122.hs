{-# OPTIONS_GHC #-}

import qualified Data.IntSet as Set
import Data.Ord
import Data.List
import Data.Array

-- http://oeis.org/A003313

normalize sets = go $ takeWhile ((==len).Set.size) sets'
  where
    sets' = sortBy (comparing Set.size) sets
    len = Set.size $ head sets'

    go [] = []
    go (x:xs) = x : go [set | set <- xs, set /= x, not $ Set.null (Set.difference x set)]

list = listArray (1,200) $ map go [1..]
  where
    go 1 = [Set.empty]
    go n = normalize lst
      where
        lst = [ Set.insert n $ si `Set.union` sj
              | i <- [1..n-1]
              , let j = n - i
              , i >= j
              , si <- list ! i
              , sj <- list ! j
              , Set.null $ Set.difference sj si
              ]

solve n = Set.size $ head $ list ! n

problem_122 = sum $ map solve [1..200]

main = print problem_122
