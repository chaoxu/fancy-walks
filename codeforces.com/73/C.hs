{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Char
import Data.Array.Unboxed
import Control.Monad
import Control.Applicative

inf = 10^9

getId :: Char -> Int
getId ch = ord ch - ord 'a'

trans :: UArray (Int,Int) Int -> Int -> UArray (Int,Int) Int -> UArray (Int,Int) Int
trans dict ch arr = accumArray max (negate inf) bnds arrList
  where
    bnds = bounds arr 
    arrList = [ ((lastCh, used), prevValue + deltaValue)
              | (lastCh, used) <- range bnds
              , let used' = used - if lastCh == ch then 0 else 1
              , used' >= 0
              , prevCh <- [0..25]
              , let prevValue  = arr  ! (prevCh, used')
                    deltaValue = dict ! (prevCh, lastCh)
              ]

solve :: [Int] -> Int -> UArray (Int,Int) Int -> Int
solve s k dict = rec initArray (tail s)
  where
    bnds = ((0,0), (25, k))
    initArray = array bnds [ ((ch, used), if used == used' then 0 else negate inf)
                           | (ch, used) <- range bnds
                           , let used' = if ch == head s then 0 else 1
                           ]
    rec arr (x:xs) = rec (trans dict x arr) xs
    rec arr [] = maximum [arr ! ind | ind <- range bnds]

parse :: [String] -> ((Int,Int),Int)
parse [x,y,c] = ((getId $ head x, getId $ head y), read c)

main = do
    [s,kS] <- words <$> getLine
    let k = read kS :: Int
    n <- read <$> getLine :: IO Int
    dict <- accumArray (flip const) 0 ((0,0),(25,25)) <$> replicateM n (parse.words <$> getLine)
    print $ solve (map getId s) k dict

