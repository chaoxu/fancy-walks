{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE RankNTypes #-}

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
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Graph
import Control.Parallel.Strategies

import Data.Array.ST
import Control.Monad.ST
import Debug.Trace

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        x <- readInteger
        y <- readInteger
        z <- readInteger
        a <- replicateM m readInteger
        return (n, m, x, y, z, a)
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

solve (n, m, x, y, z, a) = show $ countIncSubseq seq
  where
    seq :: [Int]
    seq = runST $ do
        a' <- thaw (listArray (0, m-1) a :: Array Int Integer) :: ST s (STArray s Int Integer)
        forM [0..n-1] $ \i -> do
            ai <- readArray a' (i `mod` m)
            writeArray a' (i `mod` m) ((x * ai + y * fromIntegral (i+1)) `mod` z)
            return $ fromIntegral ai

modulo :: Integral a => a
modulo = 10^9+7

newtype Mod = Mod Integer deriving Eq

instance Show Mod where
    show (Mod a) = show a

instance Num Mod where
    Mod a + Mod b = Mod $ (a + b) `mod` modulo
    Mod a - Mod b = Mod $ (a - b) `mod` modulo
    Mod a * Mod b = Mod $ (a * b) `mod` modulo
    fromInteger a = Mod $ a `mod` modulo
    abs = undefined
    signum = undefined

countIncSubseq :: [Int] -> Mod
countIncSubseq seq = runST $ do
    fenwick <- newArray bnds 0 :: ST s (STArray s Int Mod)
    s <- forM seq' $ \si -> do
        sum <- getSum fenwick bnds (si-1)
        addDelta fenwick bnds si (1 + sum)
        return (1 + sum)
    return $ sum s
  where
    s = IntSet.toList . IntSet.fromList $ seq
    m = IntMap.fromList $ zip s (range bnds)
    seq' = map (m IntMap.!) seq
    bnds = (1, length s)

lowBit :: (Bits i, Num i) => i -> i
lowBit x = x .&. negate x

getSum :: (Ix i, Bits i, Num i, Num e, MArray a e m) => a i e -> (i, i) -> i -> m e
getSum fenwick bnds x = sum `liftM` mapM (readArray fenwick) xs
  where
    xs = takeWhile (inRange bnds) $ iterate (\i -> i - lowBit i) x

addDelta :: (Ix i, Bits i, Num i, Num e, MArray a e m) => a i e -> (i, i) -> i -> e -> m ()
addDelta fenwick bnds x delta =
    forM_ xs $ \i -> do
        fi <- readArray fenwick i
        writeArray fenwick i (fi + delta)
  where
    xs = takeWhile (inRange bnds) $ iterate (\i -> i + lowBit i) x
