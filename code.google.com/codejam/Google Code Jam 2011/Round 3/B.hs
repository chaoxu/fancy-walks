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
        replicateM n readInt
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

solve :: [Int] -> Int
solve seq
    | null leftBounds = 0
    | otherwise       = answer
  where
    bnds = (0, 10000)
    count = accumArray (+) 0 (0, 10001) [(i, 1) | i <- seq] :: UArray Int Int

    leftBounds = concat [ replicate val i
                        | i <- range bnds
                        , let val = (count ! (i+1)) - (count ! (i))
                        , val > 0
                        ]

    rightBounds = concat [ replicate (-val) i
                         | i <- range bnds
                         , let val = (count ! (i+1)) - (count ! (i))
                         , val < 0
                         ]

    answer = minimum $ zipWith (-) rightBounds leftBounds
