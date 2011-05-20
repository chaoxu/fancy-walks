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
        k <- readInt
        s <- BS.unpack <$> readString
        return (k, s)
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
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show result

solve (k, s) = n * k - answer
  where
    n = length s `div` k
    bnds = ((1, 0), (n, k-1))
    grid = listArray bnds s :: UArray (Int,Int) Char

    countHT :: (Int,Int) -> Int
    countHT = (cache !)
      where
        bnds = ((0, 0), (k-1, k-1))
        cache = listArray bnds $ map go $ range bnds :: UArray (Int,Int) Int

        go (hd, tl) = length [ undefined
                             | x <- [1..n-1]
                             , let a = grid ! (x, tl)
                             , let b = grid ! (x + 1, hd)
                             , a == b
                             ]
    countChunk :: (Int,Int) -> Int
    countChunk = (cache !)
      where
        bnds = ((0, 0), (k-1, k-1))
        cache = listArray bnds $ map go $ range bnds :: UArray (Int,Int) Int

        go (prev, next) = length [ undefined
                                 | x <- [1..n]
                                 , let a = grid ! (x, prev)
                                 , let b = grid ! (x, next)
                                 , a == b
                                 ]

    costH :: Int -> Int
    costH hd = costAll (hd, 1 `shiftL` hd)
      where
        costAll :: (Int,Int) -> Int
        costAll = (cache !)
          where
            bnds = ((0, 0), (k-1, 2^k-1))
            cache = listArray bnds $ map go $ range bnds :: Array (Int,Int) Int

            amsk = 2^k-1

            go (lt, msk)
                | null lst  = countHT (hd, lt)
                | otherwise = maximum lst
              where
                lst = [ costAll (now, msk') + countChunk (lt, now)
                      | now <- [0..k-1]
                      , (msk .&. (1 `shiftL` now)) == 0
                      , let msk' = msk .|. (1 `shiftL` now)
                      ]

    answer = maximum $ map costH [0..k-1]
