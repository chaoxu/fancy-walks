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
import Data.Tree
import Data.Graph

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        replicateM m $ BS.unpack <$> readString
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, grid) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve grid)

solve :: [[Char]] -> Int
solve grid = maximum answers
  where
    mergeLine = zipWith (\x y -> if elem y "GS" then x + 1 else 0)
    gridH = tail $ scanl mergeLine (repeat 0) grid
    answers = map solveLine gridH

    solveLine :: [Int] -> Int
    solveLine heights = maximum answers
      where
        m = length heights
        popStack h = dropWhile (\x -> fst x >= h)
        toL = tail $ scanl (\st (h,p) -> (h,p):popStack h st) [(-1,-1)] $ zip heights [0..]
        toR = init $ scanr (\(h,p) st -> (h,p):popStack h st) [(-1,m)] $ zip heights [0..]
        len = zipWith (\(_:(_,lp):_) (_:(_,rp):_) -> rp - lp - 1) toL toR
        answers = zipWith (*) len heights
