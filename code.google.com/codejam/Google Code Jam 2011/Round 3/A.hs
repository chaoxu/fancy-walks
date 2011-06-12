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
import Text.Printf

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        width <- readInt
        lowerCount <- readInt
        upperCount <- readInt
        guests <- readInt
        lowers <- replicateM lowerCount ((,) <$> readInt <*> readInt)
        uppers <- replicateM upperCount ((,) <$> readInt <*> readInt)
        return (width, guests, lowers, uppers)
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
        putStrLn $ "Case #" ++ show cas ++ ":"
        forM_ result $ \r -> do
            putStrLn $ printf "%.10f" r

solve (width, guests, lowers, uppers) = [mySplit r | x <- [1..guests-1], let r = fromIntegral x / fromIntegral guests * area]
  where
    lowers' = zip lowers (tail lowers)
    uppers' = zip uppers (tail uppers)

    getArea :: ((Int, Int),(Int,Int)) -> Double -> Double
    getArea ((x1,y1),(x2,y2)) d
        | d <= fromIntegral x1   = 0
        | d >= fromIntegral x2   = fromIntegral (y1 + y2) * fromIntegral (x2 - x1)
        | otherwise              = (fromIntegral y1 + y') * (d - fromIntegral x1)
      where
        y' = (fromIntegral y1 * (fromIntegral x2 - d) + fromIntegral y2 * (d - fromIntegral x1)) / fromIntegral (x2 - x1)
    
    getUpper d = sum [getArea x d | x <- uppers']
    getLower d = sum [getArea x d | x <- lowers']

    get d = getUpper d - getLower d

    w = fromIntegral width
    area = get w

    bsearch :: (Double, Double) -> Int -> Double -> Double
    bsearch (lo, hi) times targetArea
        | times == 0           = mid
        | get mid > targetArea = bsearch (lo, mid) (times-1) targetArea
        | otherwise            = bsearch (mid, hi) (times-1) targetArea
      where
        mid = (lo + hi) / 2

    mySplit area = bsearch (0, w) 64 area
