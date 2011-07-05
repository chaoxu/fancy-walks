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
        f <- readInteger
        d <- readInt
        b <- readInt
        return (f, d, b)
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

limit :: Integer
limit = 2^32

maxFloors :: Int -> Int -> Integer
maxFloors d b
    | b == 0    = 0
    | b == 1    = x
    | b == 2    = x * (x + 1) `div` 2
    | b > n     = 2 ^ min d n - 1
    | otherwise = if inRange (bounds rb) d then rb ! d else limit
  where
    x = toInteger d
    n = 100 :: Int

    trans xs = scanl (\a b -> a + b + 1) 0 xs
    buildRow row = listArray (0, length row - 1) row :: Array Int Integer

    rows = listArray (3, n) $ map (buildRow . takeWhile (<=limit)) $ drop 2 $ iterate trans [0..] :: Array Int (Array Int Integer)

    rb = rows ! b

solve (f, d, b) = show f' ++ " " ++ show d' ++ " " ++ show b'
  where
    floors = maxFloors d b
    f' = if floors >= limit then -1 else floors

    d' = head $ dropWhile (\x -> maxFloors x b < f) [0..]
    b' = head $ dropWhile (\x -> maxFloors d x < f) [0..]
