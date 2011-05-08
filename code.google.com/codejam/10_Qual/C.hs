{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe
import Data.Char
import Data.Array
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
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Tree
import Data.Graph

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        steps <- readInt
        limit <- readInt
        n <- readInt
        groupSize <- replicateM n readInt
        return (steps, limit, groupSize)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, (steps, limit, groupSize)) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve steps limit groupSize)

type MyArray = Array Int (Int,Int64)

solve :: Int -> Int -> [Int] -> Int64
solve steps limit groupSize = snd (res ! 0)
  where
    n = length groupSize
    sum = listArray (0, 2 * n) $ scanl (+) 0 (map fromIntegral $ groupSize ++ groupSize :: [Int64])
    next p = go p (p + n)
      where
        go lo hi
            | lo == hi                                        = lo
            | sum ! (mid + 1) - sum ! p <= fromIntegral limit = go (mid + 1) hi
            | otherwise                                       = go lo mid
          where
            mid = (lo + hi) `div` 2
    bnds = (0, n - 1)

    pow0 = listArray bnds [(idx, 0) | idx <- range bnds]
    pow1 = listArray bnds $ map (\x -> let y = next x in (y `mod` n, sum ! y - sum ! x)) (range bnds) :: MyArray
    mult next1 next2 = listArray bnds [ (y, u + v)
                                      | idx <- range bnds
                                      , let (x, u) = next1 ! idx
                                      , let (y, v) = next2 ! x
                                      ]
    pows = iterate (\x -> mult x x) pow1
    sigs = map (testBit steps) [0..32]
    pows' = map fst . filter snd $ zip pows sigs
    res = foldl mult pow0 pows'
