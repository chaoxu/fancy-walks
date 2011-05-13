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
        a <- forM ([1..n] ++ [n-1,n-2..1]) $ flip replicateM readInt
        return (n, a)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve params)

solve (k, a) = (k + deltak) ^ 2 - k ^ 2
  where
    totLines = 2 * k - 1
    numOnLine i
        | i <= k    = i
        | otherwise = 2 * k - i
    leadingSpace i
        | i <= k    = k - i
        | otherwise = i - k

    none = negate 1
    bnds = ((1,1),(totLines,totLines))

    arr = accumArray (flip const) none bnds [ ((i, leadingSpace i + j * 2 - 1), ele)
                                            | (i, row) <- zip [1..] a
                                            , (j, ele) <- zip [1..] row
                                            ]

    dis (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

    checkAll (cx, cy) = and [ check (cx,cy) idx
                            | idx <- range bnds
                            , let val = arr ! idx
                            , val /= none
                            ]

    deltak = minimum [dis idx (k, k) | idx <- range bnds, checkAll idx]

    check (cx, cy) (x, y) = all check' lst
      where
        x' = cx + cx - x
        y' = cy + cy - y
        lst = [(x,y), (x',y), (x, y'), (x',y')]
        val = arr ! (x, y)
        check' idx = not (inRange bnds idx) || val' == none || val' == val
          where
            val' = arr ! idx
