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
        replicateM 3 $ (,) <$> readInteger <*> readInteger
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

distSqr (x1, y1) (x2, y2) = (x1-x2)^2 + (y1-y2)^2

solve [(x1, y1), (x2, y2), (x3, y3)]
    | not isTriangle = "not a triangle"
    | otherwise      = ab1 ++ " " ++ ab2 ++ " triangle"
  where
    isTriangle = (x2 - x1) * (y3 - y1) /= (y2 - y1) * (x3 - x1)
    [len1, len2, len3] = sort [distSqr (x1, y1) (x2, y2), distSqr (x2, y2) (x3, y3), distSqr (x1, y1) (x3, y3)]

    ab1 | len1 == len2 || len2 == len3 = "isosceles"
        | otherwise                    = "scalene"

    ab2 | len1 + len2 == len3 = "right"
        | len1 + len2 > len3  = "acute"
        | otherwise           = "obtuse"
