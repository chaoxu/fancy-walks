{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe
import Data.Char
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Int
import Data.Ratio
import Data.Bits
import Data.Function
import Data.Ord
import Control.Monad.State
import Control.Monad
import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Graph
import Control.Parallel.Strategies

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        k <- readInt
        pts <- replicateM n ((,) <$> readInt <*> readInt)
        return (k, pts)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    input <- evalState parseInput <$> BS.getContents
    let output = parMap rdeepseq solve input
    forM_ (zip [1..] output) $ \(cas, result) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show result

solve (k, pts) = binarySearch ((<=k) . minNum pts) (0, 64000 + 10)

binarySearch :: (Int -> Bool) -> (Int, Int) -> Int
binarySearch check (lo, hi)
    | lo == hi  = lo
    | check mid = binarySearch check (lo, mid)
    | otherwise = binarySearch check (mid + 1, hi)
  where
    mid = (lo + hi) `div` 2

minNum :: [(Int, Int)] -> Int -> Int
minNum pts len = minSteps ((1 `shiftL` n) - 1)
  where
    xs = map head . group . sort $ [ x + dx | x <- map fst pts, dx <- [0, -len]]
    ys = map head . group . sort $ [ y + dy | y <- map snd pts, dy <- [0, -len]]

    n = length pts

    calcMask :: (Int, Int) -> Int
    calcMask (x, y) = foldl (.|.) 0 [ 1 `shiftL` i :: Int
                                    | (i, pt) <- zip [0..] pts
                                    , inRange ((x, y), (x+len, y+len)) pt
                                    ]

    masks = map head . group . sort $ [ calcMask (x, y) | x <- xs, y <- ys]

    inf = 10^9 :: Int

    minSteps :: Int -> Int
    minSteps = (cache!)
      where
        bnds = (0, (1 `shiftL` n) - 1)
        cache = listArray bnds $ map go $ range bnds :: Array Int Int

        go 0 = 0
        go msk = foldl min inf lens + 1
          where
            lens = [ minSteps nmsk
                   | m <- masks
                   , let nmsk = msk .&. complement m
                   , nmsk /= msk
                   ]
