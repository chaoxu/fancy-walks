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
        (,) <$> (BS.unpack <$> readString) <*> (BS.unpack <$> readString)
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
        mapM putStrLn result

north = (-1, 0)
south = (1, 0)
west = (0, -1)
east = (0, 1)

turnLeft (x, y) = (-y, x)
turnRight (x, y) = (y, -x)

directions = [north, south, west, east]

walk :: Char -> ((Int, Int), (Int,Int)) -> ((Int,Int),(Int,Int))
walk 'W' ((x, y), (dx, dy)) = ((x+dx, y+dy), (dx,dy))
walk 'L' (pos, dir) = (pos, turnLeft dir)
walk 'R' (pos, dir) = (pos, turnRight dir)

walkPath :: ((Int,Int),(Int,Int)) -> String -> (((Int,Int),(Int,Int)), [((Int,Int),(Int,Int))], [(Int,Int)])
walkPath start path = (last states, concatMap snd list, map (fst.fst) list)
  where
    states = tail $ scanl (flip walk) start path

    list = [ (st, forbidden st seq)
           | (st, tl) <- zip (init states) (tails path)
           , head tl == 'W'
           , let seq = takeWhile (/='W') $ tail tl
           ]

forbidden :: ((Int,Int),(Int,Int)) -> String -> [((Int,Int), (Int,Int))]
forbidden (pos,dir) "L"  = []
forbidden (pos,dir) ""   = [(pos,turnLeft dir)]
forbidden (pos,dir) "R"  = [(pos,turnLeft dir),(pos,dir)]
forbidden (pos,dir) "RR" = [(pos,turnLeft dir),(pos,dir),(pos,turnRight dir)]

solve :: (String, String) -> [String]
solve (fromEnter, fromExit) = answer
  where
    enter = ((0,0),(south))
    (exit, forbids1, grids1) = walkPath enter fromEnter
    (_, forbids2, grids2) = walkPath (fmap (turnRight.turnRight) exit) fromExit
    forbids = forbids1 ++ forbids2
    grids = grids1 ++ grids2

    minx = minimum $ map fst grids
    maxx = maximum $ map fst grids
    miny = minimum $ map snd grids
    maxy = maximum $ map snd grids

    bnds = ((minx, miny), (maxx, maxy))
    arr = accumArray (.|.) 0 bnds [ (pos, 1 `shiftL` dirID)
                                  | (pos, dir) <- forbids
                                  , let dirID = fromJust $ elemIndex dir directions
                                  ] :: UArray (Int,Int) Int

    answer = [ [ intToDigit $ 15 - (arr ! (x,y)) | y <- [miny..maxy]] | x <- [minx..maxx]]
