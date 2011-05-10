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
    len <- readInt
    w <- readInt
    cas <- readInt
    dict <- replicateM w (BS.unpack <$> readString)
    replicateM cas $ do
        pattern <- BS.unpack <$> readString
        return (len, dict, pattern)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve params)

solve (len, dict, pattern) = 
    length $ filter checkWord dict
  where
    decodePattern [] = []
    decodePattern ('(':xs) = lo : decodePattern (tail hi)
      where
        (lo, hi) = span (/=')') xs
    decodePattern (x:xs) = [x] : decodePattern xs

    bnds = ('a', 'z')
    buildPatternArray str = accumArray (||) False bnds [(ch, True) | ch <- str]
    buildPatternAll = map buildPatternArray . decodePattern

    fastPattern = buildPatternAll pattern

    check [] [] = True
    check [] _ = False
    check _ [] = False
    check (x:xs) (y:ys) = (x ! y) && check xs ys

    checkWord = check fastPattern
