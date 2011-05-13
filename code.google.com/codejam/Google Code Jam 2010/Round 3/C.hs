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
        n <- readInt
        replicateM n $ do
            (,) <$> readInteger <*> readInteger
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

data Block = Block 
             { start :: Integer
             , end   :: Integer
             , emp   :: Integer
             , sumk2 :: Integer
             } deriving (Eq, Show)

getBlock :: (Integer, Integer, Integer) -> Block
getBlock (sumk0, sumk1, sumk2) = Block divV (divV + sumk0) (divV + sumk0 - modV) (sumk2)
  where
    sum0 = sumk0 * (sumk0 - 1) `div` 2
    (divV, modV) = (sumk1 - sum0) `divMod` sumk0
    start' = divV

extractBlock :: Block -> (Integer, Integer, Integer)
extractBlock (Block f t e k2) = (t - f, sumk1, k2)
  where
    sumk1 = (f + t) * (t - f + 1) `div` 2 - e

mergeBlock :: Block -> Block -> Block
mergeBlock b1 b2 = getBlock (x0 + x1, y0 + y1, z0 + z1)
  where
    (x0, y0, z0) = extractBlock b1
    (x1, y1, z1) = extractBlock b2

touchBlock :: Block -> Block -> Bool
touchBlock b1 b2 = (start b1 `max` start b2) <= (end b1 `min` end b2)

stepsBlock :: Block -> Integer
stepsBlock (Block f t e k2) = (k2' - k2) `div` 2
  where
    -- sum $ map (^2)[f..t] - e^2
    k2' = (t + 1 - f) * (2*f^2 + 2*f*t - f + 2*t^2 + t) `div` 6 - e^2
   
singleBlock :: (Integer, Integer) -> Block
singleBlock (p, num) = getBlock (num, num * p, num * p * p)

solve xs = sum . map stepsBlock $ go blocks
  where
    blocks = map singleBlock $ sort xs

    go [] = []
    go [a] = [a]
    go (x:y:zs)
        | touchBlock x y          = go (mergeBlock x y : zs)
        | touchBlock x (head yzs) = go (mergeBlock x (head yzs) : tail yzs)
        | otherwise               = x : yzs
      where
        yzs = go (y:zs)
