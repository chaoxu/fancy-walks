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
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Graph
import Control.Parallel.Strategies
import Control.Monad.ST
import Data.Array.ST

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        a <- readInt64
        b <- readInt64
        p <- readInt
        return (a, b, p)
  where
    readInt64 = read . BS.unpack <$> readString :: State ByteString Int64
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    input <- evalState parseInput <$> BS.getContents
    let output = parMap rdeepseq solve input
    forM_ (zip [1..] output) $ \(cas, result) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show result

generatePrimes :: Int -> [Int]
generatePrimes n = runST $ do
    isPrime <- newArray (2,n) True :: ST s (STUArray s Int Bool)
    forM_ (takeWhile (\x -> x * x <= n) [2..]) $ \i -> do
        pi <- readArray isPrime i
        when pi $ forM_ [i*i,i*(i+1)..n] $ \j -> do
            writeArray isPrime j False
    filterM (readArray isPrime) [2..n]

solve :: (Int64, Int64, Int) -> Int
solve (a, b, p) = length $ components graph
  where
    primes = dropWhile (<p) $ generatePrimes (fromIntegral $ b - a + 1)

    genAdjList p = [s, s+p..b]
      where
        s = (a + p - 1) `div` p * p

    edges = [ (fromIntegral $ h - a, fromIntegral $ t - a)
            | p <- primes
            , let lst = genAdjList $ fromIntegral p
            , not (null lst)
            , let h = head lst
            , t <- tail lst
            ]
    graph = buildG (0, fromIntegral $ b - a) edges
