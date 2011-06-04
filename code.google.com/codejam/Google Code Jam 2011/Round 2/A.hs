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
        len <- readInt
        walkSpeed <- readInt
        runSpeed <- readInt
        runLimit <- readInt
        n <- readInt
        walkWays <- replicateM n $ do
            left <- readInt
            right <- readInt
            deltaSpeed <- readInt
            return (right - left, deltaSpeed)
        return (len, walkSpeed, runSpeed, runLimit, walkWays)
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


solve (len, walkSpeed, runSpeed, runLimit, walkWays) = go (fromIntegral runLimit) sortedWays
  where
    leftLen = len - sum (map fst walkWays)
    sortedWays = sortBy (comparing snd) $ (leftLen, 0) : walkWays
    walkSpeed' = fromIntegral walkSpeed
    runSpeed' = fromIntegral runSpeed

    go :: Double -> [(Int, Int)] -> Double
    go leftRun [] = 0.0
    go leftRun ((len, delta) : xs) = go (leftRun - neck) xs + time
      where
        len' = fromIntegral len
        delta' = fromIntegral delta
        neck = min leftRun (len' / (delta' + runSpeed'))

        time = neck + (len' - neck * (delta' + runSpeed')) / (delta' + walkSpeed')
