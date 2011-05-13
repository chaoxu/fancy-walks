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
import Data.Tree
import Data.Graph

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        k <- readInt
        targetPos <- readInteger
        targetTime <- readInteger
        x <- replicateM n readInteger
        v <- replicateM n readInteger
        return (k, targetPos, targetTime, x, v)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ (solve params)

solve (k, targetPos, targetTime, x, v)
    | length reachTrue < k = "IMPOSSIBLE"
    | otherwise            = show ans
  where
    canReach xi vi = (vi * targetTime + xi) >= targetPos
    reachList = reverse $ zipWith canReach x v
    reachTrue = take k . map fst . filter snd $ zip [0..] reachList
    reachFalse = map fst . filter (not.snd) $ zip [0..] reachList
    ans = sum [1 | t <- reachTrue, f <- reachFalse, f < t]
    
