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

(<!>) = (IntMap.!)

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        replicateM n $ do
            px <- (,) <$> readInt <*> readInt
            py <- (,) <$> (succ <$> readInt) <*> (succ <$> readInt)
            return (px, py)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve params)

touch2D ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = touch1D (x1, x2) (x3, x4) && touch1D (y1,y2) (y3,y4)

touch1D (l1,r1) (l2,r2) = l1 <= r2 && l2 <= r1

touch2Dstrict (a,b) (c,d) = touch2D (a,b) (c,d) && a /= d && b /= c

solve a = maximum $ map solveComponment comps
  where
    n = length a
    arr = listArray (1,n) a
    graph = buildG (1,n) [ (x, y)
                         | x <- [1..n]
                         , y <- [1..n]
                         , touch2Dstrict (arr ! x) (arr ! y)
                         ]

    comps = map (map (arr!) . F.toList) $ components graph

    solveComponment xs = maxX + maxY - starting - 1
      where
        starting = minimum $ map (uncurry (+).fst) xs
        maxX = maximum $ map (fst.snd) xs
        maxY = maximum $ map (snd.snd) xs
