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
        a <- readInt
        b <- readInt
        return (a, b)
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
        putStrLn $ "Case #" ++ show cas ++ ": " ++ result

solve (a, b)
    | ans == 1  = "WHITE"
    | ans == 2  = "BLACK"
    | otherwise = "UNKNOWN"
  where
    a' = if a >= 16 then a `mod` 16 + 16 else a
    b' = if b >= 16 then b `mod` 16 + 16 else b

    ans = search (a', b')

search :: (Int, Int) -> Int
search = (cache!)
  where
    bnds = ((0,0),(64,64))
    cache = listArray bnds $ map go $ range bnds :: Array (Int, Int) Int

    go (w, b)
        | w + b == 1 = if w == 1 then 1 else 2
        | w + b == 0 = 0
        | otherwise  = foldl (.|.) 0 (ww ++ bb ++ wb)
      where
        ww = [search (w - 2 + 1, b) | w >= 2]
        bb = [search (w + 1, b - 2) | b >= 2]
        wb = [search (w - 1, b - 1 + 1) | w >= 1 && b >= 1]
