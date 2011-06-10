
import Control.Monad

import Data.Array.IArray
import Data.Array.Unboxed (UArray)

import Data.Int

bnds = (1, 20000000)

a :: [Int]
a = 1 : 2 : (drop 2.concat) [replicate x i | (i, x) <- zip [1..] a]

a' :: [Int64]
a' = scanl (+) 1 $ map fromIntegral a

fastA :: Int -> Int
fastA = (cache!)
  where
    cache = listArray bnds a :: UArray Int Int

fastA' :: Int -> Int64
fastA' = (cache!)
  where
    cache = listArray bnds a' :: UArray Int Int64

intervals :: [Int64]
intervals = [ len
            | x <- range bnds
            , let lo = fastA' x
            , let hi = lo + fromIntegral (fastA x) - 1
              -- a[lo..hi] = x
            , let len = (hi - lo + 1) * fromIntegral x
              -- func v = v `div` fromIntegral x + lo
            ]

maxSolvableN :: Int64
maxSolvableN = sum intervals 

solve :: Int64 -> Int64
solve n = (n - leftEnd) `div` fromIntegral x + lo
  where

    arr = listArray bnds $ scanl (+) 1 intervals :: UArray Int Int64

    bsearch (lo, hi)
        | lo == hi             = lo
        | arr ! (mid + 1) <= n = bsearch (mid + 1, hi)
        | otherwise            = bsearch (lo, mid)
      where
        mid = (lo + hi) `div` 2

    x = bsearch bnds
    leftEnd = arr ! x
    lo = fastA' x

problem_341 = sum [solve (n^3) | n <- [1..999999]]

main = print problem_341
