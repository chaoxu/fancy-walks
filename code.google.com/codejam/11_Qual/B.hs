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
import Data.Tree
import Data.Graph

bnds = (('A','A'),('Z','Z'))

solve combine opposed seq = go [] Map.empty seq
  where
    go :: String -> Map Char Int -> String -> String
    go st map [] = reverse st
    go st map (x:xs)
        | not (null st) && combRes /= ' ' = go (tail st) (Map.adjust pred (head st) map) (combRes : xs)
        | checkBomb                       = go [] Map.empty xs
        | otherwise                       = go (x:st) (Map.insertWith (+) x 1 map) xs
      where
        combRes = combine ! (head st, x)
        checkBomb = any id [Map.member y map && fromJust (Map.lookup y map) > 0 | y <- ['A'..'Z'], opposed ! (x, y)]
        

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        c <- readInt
        combine <- readCombine . map BS.unpack <$> replicateM c readString
        d <- readInt
        opposed <- readOpposed . map BS.unpack <$> replicateM d readString
        n <- readInt
        seq <- BS.unpack <$> readString
        return (combine, opposed, seq)
  where
    readInt = state $ fromMaybe (error "readInt") . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readCombine strs = accumArray (flip const) ' ' bnds $ concat [[((x,y),z),((y,x),z)] | [x,y,z] <- strs]
    readOpposed strs = accumArray (||) False bnds $ concat [[((x,y),True), ((y,x),True)] | [x,y] <- strs]

show' :: String -> String
show' arr = "[" ++ intercalate ", " (map return arr) ++ "]"

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, (combine, opposed, seq)) ->
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show' (solve combine opposed seq)
