
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Array.ST

import Control.Monad.ST
import Control.Monad
import Control.Applicative

import Data.List


buildFactor :: Int -> UArray Int Int
buildFactor n = runSTUArray $ do
    arr <- newListArray (2, n) [2..n] :: ST s (STUArray s Int Int)
    forM_ (takeWhile (\x -> x*x <= n) [2..n]) $ \i -> do
        ai <- readArray arr i
        when (ai == i) $ forM_ [i*i, i*i+i .. n] $ \j -> do
            aj <- readArray arr j
            when (i < aj) $ writeArray arr j i
    return arr

maxr = 10^7 :: Int

factors = buildFactor maxr

factorize :: Int -> [Int]
factorize n
    | fn == n   = [n]
    | otherwise = fn : factorize (n `div` fn)
  where
    fn = factors ! n

-- return all square x s.t. phi(x) = n^3 for given n
solve :: Int -> [Integer]
solve n = lst
  where
    fs = map head $ group $ factorize n

    n3 = fromIntegral n ^ 3 :: Integer

    lst = [ res
          | fs' <- subsequences fs
          , fs' /= []
          , let num = product fs'
          , let den = product $ map pred fs'
          , let (divR, modR) = n3 `divMod` fromIntegral den
          , modR == 0
          , let res = divR * fromIntegral num
          , checkSquare res (map ((^2).fromIntegral) fs')
          ]
    
    checkSquare n [] = n == 1
    checkSquare n (x:xs)
        | r == 0    = checkSquare n' (x:xs)
        | otherwise = checkSquare n xs
      where
        (n', r) = n `divMod` x

isqrt :: Integer -> Integer
isqrt n = fst . head $ dropWhile (\(a,b) -> a + 1 /= b && a /= b) $ iterate (\(_,x) -> (x, (x + div n x) `div` 2)) (-1, 1)

problem_342 = sum ans
  where
    bnds = (2 ^ 2, (10^10-1) ^ 2) :: (Integer, Integer)
    ans = [ n
          | i <- [2..maxr]
          , n2 <- solve i
          , inRange bnds n2
          , let n = isqrt n2
          ]

main = print problem_342
