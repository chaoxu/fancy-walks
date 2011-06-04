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
import Data.Array.ST
import Control.Monad.ST
import System.IO

parseInput = do 
    cas <- readInt
    replicateM cas (fromIntegral <$> readInteger)
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
        hFlush stdout

maxn :: Int
maxn = 2^20

isPrime :: UArray Int Bool
isPrime = runSTUArray $ do
    isp <- newArray (2, maxn) True :: ST s (STUArray s Int Bool)
    forM_ lst $ \i -> do
        val <- readArray isp i
        when val $ forM_ [i*i,i*i+i..maxn] $ \j -> do
            writeArray isp j False
    return isp
  where
    lst = takeWhile (\x -> x^2 <= maxn) [2..]

solve :: Int64 -> Int
solve 1 = 0
solve n = 1 + ans
  where
    ans = sum [ length $ takeWhile (<=n) powers
              | i <- takeWhile (\x -> fromIntegral x ^ 2 <= n) [2..]
              , isPrime ! i
              , let powers = iterate (*fromIntegral i) (fromIntegral i^2)
              ]
