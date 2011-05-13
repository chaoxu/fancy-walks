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
        del <- readInt
        ins <- readInt
        gap <- readInt
        a <- readInt >>= flip replicateM readInt
        return (del, ins, gap, a)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve params)

solve (del, ins, gap, a) = minimum [go' (0, i) | i <- [0..255]]
  where
    n = length a
    arr = listArray (0, n-1) a
    inf = 10^9

    bnds = ((0,0),(n,255))
    cache = listArray bnds [go idx | idx <- range bnds]
    go' = (cache!)
    go (pos, last)
        | pos == n  = 0
        | otherwise = toDel `min` toMod `min` inf
      where
        nowValue = arr ! pos

        toDel = go' (pos+1,last) + del
        toMod = minimum $ inf : [ insCost (last - newValue) + modCost + go' (pos + 1, newValue)
                                | newValue <- [0..255]
                                , let modCost = abs (newValue - nowValue)
                                ]

    insCost delta 
        | delta' == 0 = 0 
        | gap == 0    = inf
        | otherwise   = (delta' - 1) `div` gap * ins
      where
        delta' = abs delta
