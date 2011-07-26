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

import Debug.Trace

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        rounds <- readInt
        prob <- readDouble
        initial <- readInteger
        return (rounds, prob, initial)
  where
    readDouble = read . BS.unpack <$> readString :: State ByteString Double
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

solve :: (Int, Double, Integer) -> Double
solve (rounds, prob, initial) = answer
  where
    probDist = foldl solveRound [0.0, 1.0] $ replicate rounds prob

    answer = maximum [pi | (i, pi) <- zip [0..] probDist, toInteger i % (2^rounds) <= initial % (10^6)]
    
solveRound :: [Double] -> Double -> [Double]
solveRound xs p = map go [0..2*n]
  where
    n = length xs - 1
    xsCache = listArray (0, n) xs :: UArray Int Double

    go v = ternarySearch cost (lo, hi)
      where
        lo = 0 `max` (v - n)
        hi = n `min` v

        cost a = p * win + (1 - p) * lose
          where
            b = v - a

            lose = xsCache ! (min a b)
            win = xsCache ! (max a b)


ternarySearch :: Ord a => (Int -> a) -> (Int, Int) -> a
ternarySearch func (lo, hi) = go lo hi
  where
    go lo hi
        | lo + 8 > hi           = maximum [func x | x <- [lo..hi]]
        | func mid1 > func mid2 = go lo mid2
        | otherwise             = go mid1 hi
      where
        mid1 = (lo + lo + hi) `div` 3
        mid2 = (lo + hi + hi) `div` 3
