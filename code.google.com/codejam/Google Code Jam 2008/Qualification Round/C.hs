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
import Text.Printf

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        f <- readDouble
        r <- readDouble
        t <- readDouble
        r' <- readDouble
        g <- readDouble
        return (f, r, t, r', g)
  where
    readDouble = read . BS.unpack <$> readString :: State ByteString Double
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    input <- evalState parseInput <$> BS.getContents
    let output = parMap rdeepseq solve input
    forM_ (zip [1..] output) $ \(cas, result) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ printf "%.10f" result

solve (f, r, t, r', g)
    | 2 * f >= g = 1.0
    | otherwise  = 1.0 - solveArea (r - t - f, r' + f, g - 2 * f) / area
  where
    area = pi * r^2

solveArea :: (Double, Double, Double) -> Double
solveArea (r, r', g) = 4 * sum [solveSquare r (x+r',y+r') (x+r'+g,y+r'+g) | x <- [0,len..r], y <- [0,len..r]]
  where
    len = r' * 2 + g

solveSquare :: Double -> (Double,Double) -> (Double,Double) -> Double
solveSquare r (x0,y0) (x1,y1) 
    | x1^2 + y1^2 <= r^2 = (x1 - x0) * (y1 - y0)
    | x0^2 + y0^2 > r^2  = 0
    | otherwise          = solveL r (x0,y0) + solveL r (x1,y1) - solveL r (x0,y1) - solveL r (x1,y0)

solveL :: Double -> (Double,Double) -> Double
solveL r (x,y)
    | x^2+y^2 > r^2 = 0.0
    | otherwise     = (solveAbove r y - solveAbove r y') / 2 - (y' - y) * x
  where
    y' = sqrt (r^2-x^2)

solveAbove :: Double -> Double -> Double
solveAbove r y
    | y >= r    = 0
    | otherwise = acos (y / r) * r^2 - x * y
  where
    x = sqrt (r^2 - y^2)
