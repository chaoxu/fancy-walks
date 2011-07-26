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
import Data.Tuple

import Data.Graph.Inductive hiding (Gr)
import Data.Graph.Inductive.PatriciaTree

import Debug.Trace

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        tree <- replicateM (n - 1) ((,) <$> readInt <*> readInt)
        m <- readInt
        pattern <- replicateM (m - 1) ((,) <$> readInt <*> readInt)
        return (n, tree, m, pattern);
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
        putStrLn $ "Case #" ++ show cas ++ ": " ++ if result then "YES" else "NO"

solve (n, tree, m, pattern)
    | m == 1    = True
    | otherwise = answer
  where
    tree' = tree ++ map swap tree
    pattern' = pattern ++ map swap pattern

    adjTree = accumArray (flip (:)) [] (1, n) tree' :: Array Int [Int]
    adjPattern = accumArray (flip (:)) [] (1, m) pattern' :: Array Int [Int]

    treeMap = Map.fromList $ zip tree' [1..]
    patternMap = Map.fromList $ zip pattern' [1..]

    treeArr = listArray (1, length tree') tree' :: Array Int (Int,Int)
    patternArr = listArray (1, length pattern') pattern' :: Array Int (Int,Int)

    u = head [i | i <- [1..m], length (adjPattern ! i) == 1]
    v = head (adjPattern ! u)

    answer = or [matching (x, y) (u, v) | (x, y) <- tree']

    matching (x, y) (u, v) = cache ! (treeMap Map.! (x, y), patternMap Map.! (u, v))
      where
        bnds = ((1, 1), (snd $ bounds treeArr, snd $ bounds patternArr))
        cache = listArray bnds $ map go $ range bnds :: Array (Int,Int) Bool

        go (e1, e2)
            | null chdUV = True
            | otherwise  = bipartiteMatching (length chdXY, length chdUV) edges == length chdUV
          where
            (x, y) = treeArr ! e1
            (u, v) = patternArr ! e2

            chdXY = delete x (adjTree ! y)
            chdUV = delete u (adjPattern ! v)

            edges = [ (i, j)
                    | (i, z) <- zip [0..] chdXY
                    , (j, w) <- zip [0..] chdUV
                    , matching (y, z) (v, w)
                    ]

bipartiteMatching :: (Int,Int) -> [(Int,Int)] -> Int
bipartiteMatching (n, m) edges = flow
  where
    source = n + m
    target = source + 1
    edgesFromSource = [(source, i, 1) | i <- [0..n-1]]
    edgesToTarget = [(n + i, target, 1) | i <- [0..m-1]]

    edgesOther = [(x, n + y, 1) | (x, y) <- edges]

    nodes = [(i, ()) | i <- [0..target]]

    graph = mkGraph nodes (edgesFromSource ++ edgesToTarget ++ edgesOther) :: Gr () Int

    flow = maxFlow graph source target
