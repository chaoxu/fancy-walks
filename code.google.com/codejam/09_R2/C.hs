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
import Data.Graph
import Control.Parallel.Strategies

import Data.Graph.Inductive hiding (Gr)
import Data.Graph.Inductive.PatriciaTree 

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        replicateM n $ replicateM m readInt
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

solve price = n - answer
  where
    n = length price

    source = n + n
    sink = n + n + 1

    check a b = and $ zipWith (<) a b

    graph = mkGraph [(i, ()) | i <- [0..sink]] (edgesS ++ edgesT ++ edgesM) :: Gr () Int
      where
        edgesS = [ (source, i, 1) | i <- [0..n-1]]
        edgesT = [ (i + n, sink, 1) | i <- [0..n-1]]
        edgesM = [ (i, j + n, 1)
                 | (i, pi) <- zip [0..] price
                 , (j, pj) <- zip [0..] price
                 , check pi pj
                 ]

    answer = maxFlow graph source sink
