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
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Graph
import Control.Parallel.Strategies

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        x <- sort . map fromIntegral <$> replicateM n readInt :: State ByteString [Int64]
        y <- sort . map fromIntegral <$> replicateM n readInt :: State ByteString [Int64]
        return (n, x, y)
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

solve (n, x, y) 
    = mult (take ns xn) (take ns yp) + 
      mult (take ps xp) (take ps yn) + 
      mult (drop ns xn ++ drop ns yp) (reverse $ drop ps xp ++ drop ps yn)
  where
    (xn, xp) = fmap reverse $ span (<0) x
    (yn, yp) = fmap reverse $ span (<0) y

    ns = length xn `min` length yp
    ps = length xp `min` length yn

    mult a b = sum $ zipWith (*) a b
