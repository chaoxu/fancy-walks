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
        n <- readInt
        m <- readInt
        edges <- replicateM m $ (,) <$> readInt <*> readInt
        return (n, edges)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, (n, edges)) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve n edges)

solve n edges
    | comps == 1 = head oddInComps `div` 2
    | otherwise  = comps + sum [0 `max` (x - 2) | x <- oddInComps] `div` 2
  where
    swap (a,b) = (b,a)
    graph = buildG (0, n-1) (edges ++ map swap edges)
    degree = indegree graph
    appeared = filter (\x -> (degree ! x) > 0) [0..n-1]
    oddInComps = drop (n - length appeared) . sort $ map (length . filter odd . map (degree!) . F.toList) $ components graph
    comps = length oddInComps
