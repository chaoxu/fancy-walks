{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe
import Data.Char
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

import Data.Array.ST
import Control.Monad.ST
import Data.Array.Unboxed (UArray, (!))
import Data.Monoid
import Debug.Trace

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        d <- readInt
        n <- readInt
        seq <- replicateM n readInt
        return (10^d, seq)
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

data Answer a = NotFound | Ambiguous | Unique a deriving Eq

instance Show a => Show (Answer a) where
    show (Unique a) = show a
    show _ = "I don't know."

instance Eq a => Monoid (Answer a) where
    mempty = NotFound
    Ambiguous `mappend` _ = Ambiguous
    _ `mappend` Ambiguous = Ambiguous
    NotFound `mappend` a = a
    a `mappend` NotFound = a
    Unique a `mappend` Unique b = if a == b then Unique a else Ambiguous

solve :: (Int, [Int]) -> Answer Int
solve (limit, seq) = mconcat $ map (solveSingleCase seq) $ filter isPrime [2..limit]

inverseModulo :: Int -> Int -> Int
inverseModulo a prime = fromIntegral $ fastPower (fromIntegral a) (prime'-2) prime' where prime' = fromIntegral prime

fastPower :: Int64 -> Int64 -> Int64 -> Int64
fastPower a b modulo
    | b == 0    = 1
    | even b    = r2
    | otherwise = r2 * a `mod` modulo
  where
    b' = b `shiftR` 1
    r = fastPower a b' modulo
    r2 = r * r `mod` modulo

solveSingleCase :: [Int] -> Int -> Answer Int
solveSingleCase seq prime
    | maximum seq >= prime = NotFound
    | length seq == 1      = Ambiguous
    | unique seq           = Unique (head seq)
    | length seq == 2      = Ambiguous
    | found0n              = NotFound
    | unique as            = Unique ans
    | otherwise            = NotFound
  where
    -- x, y = x * a + b, z = y * x + b
    -- (y - x) * a = z - x

    plus x y = (x + y) `mod` prime
    minus x y = (x + prime - y) `mod` prime
    multi x y = fromIntegral (fromIntegral x * fromIntegral y `mod` fromIntegral prime :: Int64) :: Int

    diffSeq = zipWith minus (tail seq) seq
    instances = zip diffSeq (tail diffSeq)

    found0n = any (\(x,y) -> x == 0 && y > 0) instances
    as = map (\(x,y) -> y `multi` inverseModulo x prime) $ filter (\(x,y) -> x /= 0) instances

    solveb a = (seq !! 1) `minus` ((seq !! 0) `multi` a)

    solvenext a b = ((last seq) `multi` a) `plus` b

    a0 = head as

    ans = solvenext a0 (solveb a0)

    unique xs = and $ zipWith (==) xs (tail xs)

isPrime :: Int -> Bool
isPrime = (cache!)
  where
    n = 1000000
    cache = runSTUArray $ do
        arr <- newArray (2, n) True :: ST s (STUArray s Int Bool)
        forM_ (takeWhile ((<=n).(^2)) [2..]) $ \i -> do
            val <- readArray arr i
            when val $ forM_ [i*i,i*i+i..n] $ \j -> do
                writeArray arr j False
        return arr
