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
import Debug.Trace

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        (,) <$> readInteger <*> readInt
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

modulo :: Integral a => a
modulo = 1000000007

newtype Mod = Mod { unMod :: Int } deriving Eq

instance Show Mod where
    show (Mod v) = show v

instance Num Mod where
    Mod a + Mod b = Mod $ (a + b) `mod` modulo
    Mod a - Mod b = Mod $ (a + modulo - b) `mod` modulo
    Mod a * Mod b = Mod . fromIntegral $ (fromIntegral a * fromIntegral b :: Int64) `mod` modulo
    abs = undefined
    signum = undefined
    fromInteger v = Mod $ fromInteger v

solve (n, base) = calcWays (0, 0, 0)
  where
    base' = toInteger base
    rep' = reverse . map fromIntegral $ unfoldr (\a -> if a == 0 then Nothing else Just (a `mod` base', a `div` base')) n :: [Int]
    len = length rep'
    rep = listArray (0, len - 1) rep'

    columnSum :: (Int,Int) -> Mod
    columnSum idx = if inRange bnds idx then cache ! idx else 0
      where
        bnds = ((0,0),(base, base*base `div` 2))
        cache = foldr transArray startArray [1..base-1]

        startArray = accumArray (+) 0 bnds [((0,0),1)]

        transArray key array = accum (+) array incr
          where
            incr = [ ((num+1, sum+key), val)
                   | ((num, sum), val) <- assocs array
                   , val /= 0
                   ]

    combine :: (Int, Int) -> Mod
    combine idx = if inRange bnds idx then cache ! idx else 0
      where
        bnds = ((0, 0), (128, 128))
        cache = listArray bnds $ map go $ range bnds

        go (x, y) 
            | x < y            = 0
            | y == 0 || y == x = 1
            | otherwise        = combine (x-1, y-1) + combine (x-1, y)

    factorial :: Int -> Mod
    factorial idx = cache ! idx
      where
        bnds = (0, 128)
        cache = listArray bnds $ map go $ range bnds

        go 0 = 1
        go x = factorial (x-1) * Mod x

    calcWays :: (Int,Int,Int) -> Mod
    calcWays idx = cache ! idx
      where
        bnds = ((0, 0, 0), (len, base, base))
        cache = listArray bnds $ map go $ range bnds

        go (pos, carry, num)
            | pos == len = if carry == 0 then 1 else 0
            | otherwise  = sum lst + sum lst0
          where
            sum' = carry * base + (rep ! pos)
            lst = [ wys * calcWays (pos+1, carry', num') * combine (num', num) * factorial num
                  | carry' <- [0..base-1]
                  , carry' <= sum'
                  , num' <- [num..base-1]
                  , let wys = columnSum (num', sum' - carry')
                  , wys /= 0
                  ]
            lst0 = [ wys * calcWays (pos+1, carry', num'+1) * combine (num', num-1) * factorial num
                   | num > 0
                   , carry' <- [0..base-1]
                   , carry' <= sum'
                   , num' <- [num-1..base-1]
                   , let wys = columnSum (num', sum' - carry')
                   , wys /= 0
                   ]
