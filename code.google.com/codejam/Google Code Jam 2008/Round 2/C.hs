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
        points <- replicateM n $ Point <$> readInt <*> readInt <*> readInt <*> readInt
        return (n, points)
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

data Point = Point Int Int Int Int deriving Show

dist :: Point -> Point -> Double
dist (Point x1 y1 z1 p1) (Point x2 y2 z2 p2) = fromIntegral hDis / fromIntegral (p1 + p2)
  where
    hDis = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

solve :: (Int, [Point]) -> Double
solve (n, pts) = maximum dists
  where
    dists = [ dist pi pj
            | (i, pi) <- zip [0..] pts
            , pj <- take i pts
            ] ++ [0.0]
