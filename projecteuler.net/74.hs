{-# OPTIONS_GHC -O2 #-}

import Data.Array
import Data.Char
import Data.Set (empty, member, insert)

limit = 3000000

factorial :: Int -> Int
factorial n = product [1..n]
factCache = array (0,9) [(i,factorial i) | i <- [0..9]]
factorial' = (factCache !)

next :: Int -> Int
next = sum . map (factorial'.digitToInt) . show 
nextCache = array (1,limit) [(i, next i) | i <- [1..limit]]
next' = (nextCache !)

check limit n = go 0 n empty
  where
    go step now set | step == limit = member now set
    go step now set | member now set = False
    go step now set = go (step + 1) (next' now) (insert now set)
    
problem_74 = length $ filter (check 60) [1..1000000]

main = print problem_74
