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
        a <- replicateM n (replicateM m readInt)
        return (n, m, a)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStr $ "Case #" ++ show cas ++ ":\n" ++ (solve params)

solve (n, m, a) = unlines . map (unwords.map (\x->[x])) $ output
  where
    bnds = ((1,1),(n,m))
    arr = listArray bnds [ele | row <- a, ele <- row]

    next (x,y) = ans
      where
        delta = [(0,0),(-1,0),(0,-1),(0,1),(1,0)]
        pts = [pt | (dx,dy) <- delta, let pt = (x+dx,y+dy), inRange bnds pt]
        ans = minimumBy (compare `on` (arr!)) pts

    graph = buildG (0, rangeSize bnds-1) [(index bnds idx, index bnds $ next idx) | idx <- range bnds]

    comps = sort $ map (sort . F.toList) $ components graph

    colored = array (0, rangeSize bnds-1) [ (idx, color)
                                          | (color, comp) <- zip ['a'..] comps
                                          , idx <- comp
                                          ]

    output = [[colored ! (index bnds (i,j)) | j <- [1..m]] | i <- [1..n]]
