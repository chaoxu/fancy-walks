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

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        (,) <$> readInt <*> readInt
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve params)

combine :: (Int,Int) -> Integer
combine idx = if inRange bnds idx then cache ! idx else 0
  where
    bnds = ((0,0),(100,100))
    cache = listArray bnds $ map go $ range bnds

    go (_,0) = 1
    go (x,y) | x == y = 1
    go (x,y) | x < y = 0
    go (x,y) = combine (x-1, y) + combine (x-1, y-1)

solve (n, m) = fromRational $ simulate 0 :: Double
  where
    total = combine (n, m)
    simulate :: Int -> Rational
    simulate = (cache !)
      where
        bnds = (0, n)
        cache = listArray bnds $ map go $ range bnds
        
        go i | i == n = 0
        go i = 1 / (1 - prob0) + sum [prob / (1-prob0) * simulate j | (prob, j) <- zip (tail lst) [i+1..n]]
          where
            lst = [(combine (i,m-x) * combine (n-i,x)) % total | x <- [0..m]]
            prob0 = head lst
