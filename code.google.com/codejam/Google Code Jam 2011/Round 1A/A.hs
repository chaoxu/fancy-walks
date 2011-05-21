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
        n <- readInteger
        pd <- readInteger
        pg <- readInteger
        return (n, pd, pg)
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

solve :: (Integer, Integer, Integer) -> String
solve (n, pd, pg)
    | n < todayPlayed                             = "Broken"
    | todayWin > 0 && percentG == fromIntegral 0  = "Broken"
    | todayLose > 0 && percentG == fromIntegral 1 = "Broken"
    | otherwise                                   = "Possible"
  where
    percentG = pg % 100
    percentD = pd % 100

    todayPlayed = denominator percentD
    todayWin = numerator percentD
    todayLose = todayPlayed - todayWin
