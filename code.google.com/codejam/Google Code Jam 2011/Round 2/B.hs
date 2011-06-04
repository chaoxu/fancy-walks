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
import System.IO

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        mass <- readInt
        grid <- replicateM n (BS.unpack <$> readString)
        return (n, m, grid)
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
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show' result
        hFlush stdout
  where
    show' Nothing = "IMPOSSIBLE"
    show' (Just a) = show a

calcSum :: UArray (Int,Int) Int64 -> (Int,Int) -> Int64
calcSum arr = go'
  where
    bnds = bounds arr
    cache = listArray bnds $ map go $ range bnds :: Array (Int,Int) Int64

    go' (x, y) | x < 1 || y < 1 = 0
    go' idx = cache ! idx

    go (x, y) = go' (x - 1, y) + go' (x, y - 1) - go' (x - 1, y - 1) + (arr ! (x, y))

getSum :: ((Int,Int) -> Int64) -> (Int, Int) -> (Int, Int) -> Int64
getSum func (lx', ly') (rx, ry) = func (lx, ly) + func (rx, ry) - func (lx, ry) - func (rx, ly)
  where
    lx = lx' - 1
    ly = ly' - 1

getCornerSum :: UArray (Int,Int) Int64 -> (Int,Int) -> (Int,Int) -> Int64
getCornerSum arr (lx, ly) (rx, ry) = sum [arr ! (x, y) | x <- [lx, rx], y <- [ly, ry]]

solve :: (Int, Int, [String]) -> Maybe Int
solve (n, m, grid) = listToMaybe [ size 
                                 | size <- reverse [3 .. min n m]
                                 , x <- [1..n-size+1]
                                 , y <- [1..m-size+1]
                                 , check size x y
                                 ]
  where
    bnds = ((1,1),(n,m))

    gridw = listArray bnds [fromIntegral $ ord ele - ord '0' | row <- grid, ele <- row] :: UArray (Int,Int) Int64
    gridx = array bnds [((i,j), ele * fromIntegral i) | ((i,j), ele) <- assocs gridw] :: UArray (Int,Int) Int64
    gridy = array bnds [((i,j), ele * fromIntegral j) | ((i,j), ele) <- assocs gridw] :: UArray (Int,Int) Int64

    gridwS = calcSum gridw
    gridxS = calcSum gridx
    gridyS = calcSum gridy

    check :: Int -> Int -> Int -> Bool
    check size x y = checkX && checkY
      where
        cornerUL = (x, y)
        cornerBR = (x + size - 1, y + size - 1)
        sumw = getSum gridwS cornerUL cornerBR - getCornerSum gridw cornerUL cornerBR
        sumx = getSum gridxS cornerUL cornerBR - getCornerSum gridx cornerUL cornerBR
        sumy = getSum gridyS cornerUL cornerBR - getCornerSum gridy cornerUL cornerBR

        checkX = sumw * fromIntegral (x + x + size - 1) == 2 * sumx
        checkY = sumw * fromIntegral (y + y + size - 1) == 2 * sumy
