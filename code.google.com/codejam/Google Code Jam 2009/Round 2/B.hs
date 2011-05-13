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
import Data.Graph
import Control.Parallel.Strategies
import Debug.Trace

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        maxFall <- readInt
        grid <- replicateM n $ BS.unpack <$> readString
        return (n, m, maxFall, grid)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    input <- evalState parseInput <$> BS.getContents
    let output = parMap rdeepseq solve input
    forM_ (zip [1..] output) $ \(cas, result) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ result

solve (n, m, maxFall, grid') = if answer == maxBound then "No" else "Yes " ++ show answer
  where
    bnds = ((0,0),(n-1,m-1))
    grid = listArray bnds [ele == '#' | row <- grid', ele <- row]

    fall :: (Int,Int) -> Int
    fall = (cache!)
      where
        cache = listArray bnds $ map go $ range bnds

        go (x,y)
            | x == n - 1 = 0
            | below      = 0
            | otherwise  = 1 + go (x + 1, y)
          where
            below = grid ! (x + 1, y)

    answer = search (0,0,0)
    
    search :: (Int,Int,Int) -> Int
    search = (cache!)
      where
        bnds = ((0,0,0),(n-1,m-1,m-1))
        cache = listArray bnds $ map go $ range bnds
        
        go (x, y, y')
            | x == n - 1      = 0
            | fall (x,y) /= 0 = error "search: impossible"
            | answers == []   = maxBound
            | otherwise       = minimum answers
          where
            rng = if y <= y' then (y, y') else (y', y)
            check p = fall (x, p) == 0 && (not (grid ! (x, p)) || inRange rng p)
            toLeft = takeWhile check [y,y-1..0]
            toRight = takeWhile check [y..m-1]
            walkRange = reverse (tail toLeft) ++ toRight

            dig = [ step + abs (from - to)
                  | from <- walkRange
                  , to <- walkRange
                  , from /= to
                  , let from' = if to > from then from + 1 else from - 1
                  , let falled = fall (x + 1, from') + 1
                  , falled <= maxFall
                  , let to' = if falled == 1 then to else from'
                  , let step = search (x + falled, from', to')
                  , step /= maxBound
                  ]

            jump = [ step
                   | p <- [head walkRange - 1, last walkRange + 1]
                   , 0 <= p && p < m
                   , not (grid ! (x, p)) || inRange rng p
                   , let falled = fall (x, p)
                   , falled >= 1 && falled <= maxFall
                   , let step = search (x + falled, p, p)
                   , step /= maxBound
                   ]

            answers = dig ++ jump
