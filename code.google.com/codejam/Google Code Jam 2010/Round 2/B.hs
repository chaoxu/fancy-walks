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
        missUpper <- replicateM (2^n) readInt
        price <- forM [1..n] $ \i -> do
            replicateM (2^(n-i)) readInteger
        return (n, missUpper, concat $ reverse price)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve params)

data BInt = Infinite Bool | Finite Integer deriving Eq

instance Show BInt where
    show (Infinite True) = "Postive Infinite"
    show (Infinite False) = "Negative Infinite"
    show (Finite a) = show a

instance Num BInt where
    Infinite v + _ = Infinite v
    _ + Infinite v = Infinite v
    Finite a + Finite b = Finite (a + b)

    _ * Finite 0 = 0
    Finite 0 * _ = 0
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

instance Ord BInt where
    Finite a `compare` Finite b = compare a b
    Finite _ `compare` Infinite v = if v then LT else GT
    Infinite v `compare` Finite _ = if v then GT else LT
    Infinite a `compare` Infinite b
        | a == b    = EQ
        | a         = GT
        | otherwise = LT

solve (n, missUpper', price') = minCost (1, 0)
  where
    missUpper = listArray (2^n, 2^(n+1)-1) missUpper'
    price = listArray (1, 2^n-1) price'

    minCost :: (Int, Int) -> BInt
    minCost = (cache !)
      where
        sep = 2^n
        bnds = ((1,0), (2^(n+1)-1,n))
        cache = listArray bnds $ map go $ range bnds

        go (match, missed)
            | match >= sep = if missed > missUpper ! match then Infinite True else 0
            | otherwise    = costBuy `min` costNot
          where
            costBuy = minCost (match * 2, missed) + minCost (match * 2 + 1, missed) + fromInteger (price ! match)
            costNot = minCost (match * 2, missed + 1) + minCost (match * 2 + 1, missed + 1)
