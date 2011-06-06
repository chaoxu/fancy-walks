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
        n <- readInt
        grid <- replicateM n (BS.unpack <$> readString)
        return (n, grid)
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
        putStr $ "Case #" ++ show cas ++ ": \n" ++ (unlines . map show) result

solve :: (Int, [String]) -> [Double]
solve (n, grid) = map fromRational $ elems rpi
  where
    bnds = ((1,1),(n,n))
    gridA = listArray bnds [if ele == '.' then Nothing else Just (ele == '1') | row <- grid, ele <- row] :: Array (Int,Int) (Maybe Bool)

    edges = listArray (1,n) [ (win, ops)
                            | i <- [1..n]
                            , let ops = [j | j <- [1..n], isJust $ gridA ! (i, j)]
                            , let win = [j | j <- ops, Just True == gridA ! (i, j)]
                            ] :: Array Int ([Int], [Int])

    wp' = amap (\(win,ops) -> (genericLength win, genericLength ops)) edges
    wp = amap (\(win,ops) -> genericLength win % genericLength ops) edges
    
    owp = listArray (1, n) [ sum a / fromIntegral (length a)
                           | i <- [1..n]
                           , let op = snd $ edges ! i
                           , let a = map (\x -> let (won,all) = wp' ! x in (won - if fromJust $ gridA ! (x, i) then 1 else 0) % (all - 1)) op
                           ] :: Array Int Rational

    oowp = listArray (1, n) [ sum a / fromIntegral (length a)
                            | i <- [1..n]
                            , let op = snd $ edges ! i
                            , let a = map (owp !) op
                            ] :: Array Int Rational

    rpi = listArray (1, n) [ (wp ! i + oowp ! i) / fromIntegral 4 + (owp ! i) / fromIntegral 2 | i <- [1..n]] :: Array Int Rational
