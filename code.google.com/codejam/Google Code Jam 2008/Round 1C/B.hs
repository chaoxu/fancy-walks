{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe
import Data.Char
import Data.Array
import Data.Int
import Data.Word
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
        BS.unpack <$> readString
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

modulo :: Integral a => a
modulo = 2 * 3 * 5 * 7

newtype Mod = Mod Int deriving Eq

instance Show Mod where
    show (Mod a) = show a

instance Num Mod where
    Mod a + Mod b = Mod $ (a + b) `mod` modulo
    Mod a - Mod b = Mod $ (a - b) `mod` modulo
    Mod a * Mod b = Mod $ (a * b) `mod` modulo
    fromInteger a = Mod $ fromInteger a `mod` modulo
    signum = undefined
    abs = undefined

instance Ord Mod where
    Mod a `compare` Mod b = a `compare` b

instance Ix Mod where
    range (Mod a, Mod b) = map Mod $ range (a, b)
    index (Mod a, Mod b) (Mod c) = index (a, b) c
    inRange (Mod a, Mod b) (Mod c) = inRange (a, b) c

instance Bounded Mod where
    maxBound = Mod $ modulo - 1
    minBound = Mod 0

isUgly :: Mod -> Bool
isUgly (Mod a) = any (\x -> a `mod` x == 0) [2, 3, 5, 7]

solve :: [Char] -> Word64
solve num = countWays (1, 0, True, Mod $ digitToInt $ head num)
  where
    n = length num
    a = listArray (0, n-1) num

    countWays :: (Int,Mod,Bool,Mod) -> Word64
    countWays = (cache !)
      where
        bnds = ((1, minBound, False, minBound), (n, maxBound, True, maxBound))
        cache = listArray bnds $ map go $ range bnds

        func True = (+)
        func False = (-)

        go (pos, pre, isPlus, now) 
            | pos == n  = if isUgly mergePreNow then 1 else 0
            | otherwise = waysCon + waysPlus + waysMinus
          where
            mergePreNow = func isPlus pre now
            nowDigit = Mod $ digitToInt (a ! pos)

            waysCon = countWays (pos + 1, pre, isPlus, now * 10 + nowDigit)
            waysPlus = countWays (pos + 1, mergePreNow, True, nowDigit)
            waysMinus = countWays (pos + 1, mergePreNow, False, nowDigit)
