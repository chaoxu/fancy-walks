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
        info <- replicateM n ((,) <$> readPair <*> (BS.unpack . BS.dropWhile isSpace <$> readLine))
        m <- readInt
        query <- replicateM m readPair
        return (info, query)
  where
    readPair = (,) <$> readInt <*> readInt
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

inf = 10^9 :: Int

addPoint (minx,maxx,miny,maxy) (x,y) = (min minx x, max maxx x, min miny y, max maxy y)
inRect (minx,maxx,miny,maxy) (x,y) = minx <= x && x <= maxx && miny <= y && y <= maxy

solve :: ([((Int,Int),String)], [(Int,Int)]) -> [String]
solve (info, query) = map getType query
  where
    (birds', notBirds') = partition ((=="BIRD").snd) info
    birds = map fst birds'
    notBirds = map fst notBirds'

    rect = foldl addPoint (inf,-inf,inf,-inf) birds

    checkBird (x,y) = let rect' = addPoint rect (x,y) in and [not $ inRect rect' pos | pos <- notBirds]
    checkNotBird (x,y) = not $ inRect rect (x,y)

    getType pos
        | isBird && notBird = "UNKNOWN"
        | isBird            = "BIRD"
        | notBird           = "NOT BIRD"
        | otherwise         = error $ "solve.getType " ++ show pos
      where
        isBird = checkBird pos
        notBird = checkNotBird pos
