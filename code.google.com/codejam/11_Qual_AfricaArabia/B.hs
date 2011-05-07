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

solve m a
    | ans <= 0  = "IMPOSSIBLE"
    | otherwise = show (ax + 1) ++ " " ++ show (ay + 1) ++ " " ++ show ans
  where
    ((ans, _), ax, ay) = maximum [(go m (a !! x) (a !! y), x, y) | x <- [0..10], y <- [x+1..11]]
    go m x y = ((y - x) * (m `div` x), -x)

parseInput = do 
    cas <- readInt
    replicateM cas $ (,) <$> readInt <*> replicateM 12 readInt
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, (m,a)) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ solve m a
