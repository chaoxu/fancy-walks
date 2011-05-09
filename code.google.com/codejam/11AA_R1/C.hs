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

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        replicateM n $ (,) <$> readInt <*> readInt
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, a) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve a)

solve a = fst $ iterate trans (0.0, 1.0e9) !! 500
  where
    pts = map (\(p,t) -> (fromIntegral p, fromIntegral t)) $ sortBy (comparing snd) a :: [(Double, Double)]

    trans (lo, hi)
        | check mid = (lo, mid)
        | otherwise = (mid, hi)
      where
        mid = (lo + hi) / 2

    check d = l < r
      where
        (l, r) = go pts

        go [(p,t)] = (p - d, p + d)
        go xs@((p,t):(_,t'):_) = if l >= r then (l, r) else (l', r')
          where
            dt = t' - t
            (l, r) = go (tail xs)
            l' = max (p - d) (l - dt)
            r' = min (p + d) (r + dt)
