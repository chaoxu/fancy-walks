{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe
import Data.Char
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
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
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Graph
import Control.Parallel.Strategies

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        desired <- readBool
        interior <- replicateM (n `div` 2) $ (,) <$> readInt <*> readBool
        leaf <- replicateM (n `div` 2 + 1) $ readBool
        return (n, desired, interior, leaf)
  where
    readBool = (/=0) <$> readInt
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

solve :: (Int, Bool, [(Int, Bool)], [Bool]) -> String
solve (n, desired, interior, leaf) = show $ minStep (1, desired)
  where
    arrFunc = listArray (1, n `div` 2) $ map fst interior :: Array Int Int
    arr = listArray (1, n) $ map snd interior ++ leaf :: Array Int Bool


    minStep :: (Int, Bool) -> BoundedInteger
    minStep = (cache !)
      where
        bnds = ((1, False), (n, True))
        cache = listArray bnds $ map go $ range bnds :: Array (Int, Bool) BoundedInteger

        go (p, bool) | p * 2 > n = if arr ! p == bool then 0 else maxBound
        go (p, bool) 
            | arr ! p   = minimum $ map (+1) costChange ++ costKeep
            | otherwise = minimum costKeep
          where
            leftNode = p * 2
            rightNode = p * 2 + 1
            costAnd = [ minStep (leftNode, lbool) + minStep (rightNode, rbool)
                      | lbool <- [False, True]
                      , rbool <- [False, True]
                      , (lbool && rbool) == bool
                      ]
            costOr = [ minStep (leftNode, lbool) + minStep (rightNode, rbool)
                     | lbool <- [False, True]
                     , rbool <- [False, True]
                     , (lbool || rbool) == bool
                     ]
            (costKeep, costChange) = if arrFunc ! p == 1 then (costAnd, costOr) else (costOr, costAnd)

data BoundedInteger = Infinite Bool | Finite Integer deriving Eq

instance Show BoundedInteger where
    show (Infinite True) = "IMPOSSIBLE" -- "Postive Infinite"
    show (Infinite False) = "Negative Infinite"
    show (Finite a) = show a

instance Num BoundedInteger where
    Infinite v + _ = Infinite v -- +oo + -oo ? aha, I'm crazy
    _ + Infinite v = Infinite v
    Finite a + Finite b = Finite (a + b)

    Finite 0 * _ = 0
    _ * Finite 0 = 0
    Infinite v * a = Infinite (v == (a > 0))
    a * Infinite v = Infinite (v == (a > 0))
    Finite a * Finite b = Finite (a * b)

    negate (Infinite v) = Infinite (not v)
    negate (Finite v) = Finite (negate v)

    signum (Infinite True) = 1
    signum (Infinite False) = -1
    signum (Finite a) = Finite $ signum a

    abs (Infinite _) = Infinite True
    abs (Finite v) = Finite (abs v)

    fromInteger = Finite

instance Ord BoundedInteger where
    Finite a `compare` Finite b = compare a b
    Finite _ `compare` Infinite v = if v then LT else GT
    Infinite v `compare` Finite _ = if v then GT else LT
    Infinite a `compare` Infinite b
        | a == b    = EQ
        | a         = GT
        | otherwise = LT

instance Bounded BoundedInteger where
    minBound = Infinite False
    maxBound = Infinite True
