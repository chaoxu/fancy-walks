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

import Data.Hashable
import qualified Data.HashMap.Lazy as HM

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        grid <- replicateM n $ BS.unpack <$> readString
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

type Boxes = [(Int,Int)]
type Grid = Array (Int,Int) Char

isConnected :: Boxes -> Bool
isConnected boxes = length (components graph) == 1
  where
    isAdjBox :: (Int, Int) -> (Int, Int) -> Bool
    isAdjBox (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) == 1
    edgePairs = [ (i, j)
                | (i, bi) <- zip [0..] boxes
                , (j, bj) <- zip [0..] $ take i boxes
                , isAdjBox bi bj
                ]
    graph = buildG (0, length boxes - 1) edgePairs

isEmptyCell :: Grid -> Boxes -> (Int,Int) -> Bool
isEmptyCell grid boxes place = inRange (bounds grid) place && grid ! place /= '#' && not (elem place boxes)

solve (n, m, grid')
    | isNothing result = (-1)
    | otherwise        = pred . length $ fromJust result
  where
    bnds = ((0,0),(n-1,m-1))

    grid = listArray bnds [ele | row <- grid', ele <- row] :: Grid
    source = (True, sort $ filter (\idx -> elem (grid ! idx) "ow") $ range bnds)
    target = (True, sort $ filter (\idx -> elem (grid ! idx) "xw") $ range bnds)

    result = bfs source target (stateExpand grid)

stateExpand :: Grid -> (Bool, Boxes) -> [(Bool, Boxes)]
stateExpand grid (isCon, boxes)
    = [ (isCon', boxes')
      | (x, y) <- boxes
      , (dx, dy) <- directions
      , isEmptyCell grid boxes (x + dx, y + dy)
      , isEmptyCell grid boxes (x - dx, y - dy)
      , let boxes' = sort $ (x + dx, y + dy ) : delete (x,y) boxes 
      , let isCon' = isConnected boxes'
      , isCon || isCon'
      ]
  where
    directions = [(-1,0),(1,0),(0,-1),(0,1)]

bfs :: (Eq s, Hashable s) => s -> s -> (s -> [s]) -> Maybe [s]
bfs source target expand = go (HM.singleton source [source]) (Seq.singleton source)
  where
    go hashMap queue
        | Seq.null queue = Nothing
        | s == target    = Just paths
        | otherwise      = go hashMap' queue'
      where
        (s Seq.:< qtail) = Seq.viewl queue
        paths = fromJust $ HM.lookup s hashMap
        ts = filter (\k -> isNothing $ HM.lookup k hashMap) $ expand s
        hashMap' = foldl (\map t -> HM.insert t (t:paths) map) hashMap ts
        queue' = qtail Seq.>< Seq.fromList ts
