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

data Robot = Orange | Blue deriving (Eq, Show)

solve :: [(Robot, Int)] -> Int
solve a = go 1 1 1
  where
    nothing = negate 1
    n = length a
    arr = listArray (1, n) a
    arrO = listArray (1, n) $ scanr (check Orange) nothing a
    arrB = listArray (1, n) $ scanr (check Blue  ) nothing a
    check a (b, c) _ | a == b = c
    check _ _ d = d

    go x y s
        | s > n          = 0
        | side == Orange = let steps = 1 + abs (x - tx) in steps + go tx (move y ty steps) (s + 1) 
        | otherwise      = let steps = 1 + abs (y - ty) in steps + go (move x tx steps) ty (s + 1)
      where
        (side, target) = arr ! s
        tx = arrO ! s
        ty = arrB ! s

    move a b s
        | b == nothing = a
        | a < b        = a + min s (b - a)
        | otherwise    = a - min s (a - b)

parseInput = do 
    cases <- readInt
    input <- replicateM cases $ do 
        n <- readInt
        replicateM n $ (,) <$> (readRobot . BS.unpack <$> readString) <*> readInt
    return input
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readRobot s
        | s == "O"  = Orange
        | s == "B"  = Blue
        | otherwise = error "readRobot: undefined"

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, a) ->
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve a)
