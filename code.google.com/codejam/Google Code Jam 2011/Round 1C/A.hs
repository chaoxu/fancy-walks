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
        m <- readInt
        replicateM n (BS.unpack <$> readString)
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
        putStrLn $ "Case #" ++ show cas ++ ":"
        mapM_ putStrLn result

transform :: [String] -> [String]
transform grid = ngrid
  where
    n = length grid
    m = length (head grid)
    sharps = go $ Set.fromList [(i, j) | (i, row) <- zip [1..] grid, (j, ele) <- zip [1..] row, ele == '#', i < n && j < m]

    ngrid = [ [ if null toPaint then ele else if ele == '.' then '#' else head toPaint
              | (j, ele) <- zip [1..] row
              , let toPaint = [if dx + dy /= 1 then '/' else '\\'| dx <- [0,1], dy <- [0,1], Set.member (i-dx, j-dy) sharps]
              ]
            | (i, row) <- zip [1..] grid
            ]

    go remain = case Set.minView remain of
        Nothing -> Set.empty
        Just ((x,y), remain') -> Set.insert (x,y) $ go (remain' `Set.difference` Set.fromList [(x + 1, y), (x, y + 1), (x + 1, y + 1), (x+1, y-1)])

solve grid
    | any (=='#') $ concat ngrid = ["Impossible"]
    | otherwise                  = ngrid
  where
    ngrid = transform grid
