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

import Data.Graph.Inductive hiding (Gr)
import Data.Graph.Inductive.PatriciaTree

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        grid <- replicateM n (BS.unpack <$> readString)
        return (n, m, grid)
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

solve :: (Int, Int, [String]) -> Int
solve (n, m, grid') = numNodes - 2 - flow
  where
    bnds = ((1,1),(n,m))
    grid = listArray bnds [ele == '.' | row <- grid', ele <- row] :: UArray (Int,Int) Bool

    validSquares = zip [1..] [idx | idx <- range bnds, grid ! idx]
    nodeLabel = array bnds [(idx, i) | (i, idx) <- validSquares] :: UArray (Int,Int) Int

    numNodes = length validSquares + 2
    sourceNode = numNodes - 1
    sinkNode = numNodes

    nodes = [ (i, ()) | i <- [1..numNodes]]
    edgesFromSource = [ (sourceNode, i, 1) | (i, (x, y)) <- validSquares, even y]
    edgesToSink = [ (i, sinkNode, 1) | (i, (x, y)) <- validSquares, odd y]

    edgesOther = [ (i, j, 1)
                 | (i, (x, y)) <- validSquares
                 , even y
                 , dx <- [-1..1]
                 , dy <- [-1,1]
                 , let nidx = (x + dx, y + dy)
                 , inRange bnds nidx && grid ! nidx
                 , let j = nodeLabel ! nidx
                 ]

    graph = mkGraph nodes (edgesFromSource ++ edgesToSink ++ edgesOther) :: Gr () Int

    flow = maxFlow graph sourceNode sinkNode
    
