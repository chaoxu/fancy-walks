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
            x1 <- readInt
            y1 <- readInt
            x2 <- succ <$> readInt
            y2 <- succ <$> readInt
            return ((x1, x2), (y1, y2))
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve params)

solve a = finishDeath
  where
    uniqueCord v = Set.toList . Set.fromList $ map fst v ++ map snd v
    xs = uniqueCord $ map fst a
    ys = uniqueCord $ map snd a

    arrX = listArray (1,length xs) xs
    arrY = listArray (1,length ys) ys
    mapX = IntMap.fromList $ zip xs [1..]
    mapY = IntMap.fromList $ zip ys [1..]

    events = accumArray (flip (:)) [] (bounds arrX) $ concat [ [ (x1', (y1', y2', 1)), (x2', (y1', y2', -1))]
                                                             | ((x1, x2), (y1, y2)) <- a
                                                             , let x1' = mapX <!> x1
                                                             , let x2' = mapX <!> x2
                                                             , let y1' = mapY <!> y1
                                                             , let y2' = mapY <!> y2
                                                             ]

    scanLine arr lst = accum (+) arr [ (y, delta)
                                     | (y1, y2, delta) <- lst
                                     , y <- [y1 .. y2-1]
                                     ]

    bnds = ((1,1), (length xs,length ys))
    
    raw = listArray bnds [elem > 0 | row <- rows, elem <- row]
      where
        emptyArray = listArray (1, length ys) (repeat 0)
        rowsArr = tail $ scanl scanLine emptyArray $ map snd $ assocs events
        rows = map (map snd . assocs) rowsArr

    growed = listArray bnds [ here || upper && left
                            | (x,y) <- range bnds
                            , let upper = x > 1 && raw ! (x - 1, y)
                            , let left = y > 1 && raw ! (x, y - 1)
                            , let here = raw ! (x, y)
                            ]

    startDeath = listArray bnds [ if growed ! (x,y) then upperValue `max` leftValue else 0
                                | (x, y) <- range bnds
                                , let upper = x > 1 && growed ! (x - 1, y)
                                , let left = y > 1 && growed ! (x, y - 1)
                                , let upperValue = if upper then arrX ! x - arrX ! (x - 1) + startDeath ! (x - 1, y) else 0
                                , let leftValue = if left then arrY ! y - arrY ! (y - 1) + startDeath ! (x, y - 1) else 0
                                ]
    
    finishDeath = maximum [ if growed ! (x-1, y-1) then start + (deltaX + deltaY - 1) else 0
                          | (x, y) <- range bnds
                          , x > 1 && y > 1
                          , let start = startDeath ! (x - 1, y - 1)
                          , let deltaX = arrX ! x - arrX ! (x - 1)
                          , let deltaY = arrY ! y - arrY ! (y - 1)
                          ]
