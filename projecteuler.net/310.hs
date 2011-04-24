{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Array
import Data.Bits

mix :: [Int] -> Int
mix xs = head $ filter (\k -> not (checklist!k)) [0..]
  where
    n = length xs
    checklist = accumArray (||) False (0, n) $ zip (filter (<=n) xs) (repeat True)

-- generate the 'SG' function for [0..n]
spragueGrundy :: Int -> Int -> Int
spragueGrundy n = (arr !)
  where
    bnds = (0, n)
    arr = listArray bnds [solve i | i <- range bnds] :: Array Int Int
    solve i = mix $ map (arr!) $ takeWhile (>=0) [i - j * j | j <- [1..]]

problem_310 n = single + double + triple
  where
    sg = spragueGrundy n
    maxv = maximum $ map sg [0..n]
    len = head $ dropWhile (<=maxv) [2^i | i <- [0..]]
    bnds = (0, len - 1)
    countlist = accumArray (+) 0 bnds $ zip (map sg [0..n]) (repeat 1)
    trans arr = accumArray (+) 0 bnds inc :: Array Int Integer
      where
        inc = [(i `xor` j, (arr ! i) * (countlist ! j)) | i <- range bnds, j <- range bnds]
    st0 = listArray bnds (1:repeat 0) :: Array Int Integer
    st1 = trans st0
    st3 = trans $ trans st1
    -- target = triple + double + single
    -- st3!0  = triple * 6 + double * 3 + single
    single = st1 ! 0                                   -- number of SG(a,a,a) = 0
    double = single * toInteger n                      -- number of SG(a,b,b) = 0, a /= b
    triple = ((st3 ! 0) - double * 3 - single) `div` 6 -- number of SG(a,b,c) = 0, a < b < c

main = print $ problem_310 100000
