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
import Control.Parallel.Strategies
import Data.Complex
import Debug.Trace

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        replicateM n $ do
            x <- readDouble
            y <- readDouble
            r <- readDouble
            return (x :+ y, r)
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
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show result

eps = 1.0e-13 :: Double

type Point = Complex Double
type Circle = (Point, Double)

scale :: Point -> Double -> Point
scale (x:+y) d = (x*d) :+ (y*d)

encloseTwoCircle :: Circle -> Circle -> Double -> [Point]
encloseTwoCircle (p1,r1) (p2,r2) r
    | r1 > r || r2 > r    = []
    | not canFormTriangle = []
    | otherwise           = [center + scale (vecUnit * dir) (r1' * sinangle1) | dir <- [0 :+ 1, 0 :+ (-1)]]
  where
    r1' = r - r1
    r2' = r - r2
    p1p2 = magnitude (p1 - p2)
    sorted = sort [r1', r2', p1p2]
    canFormTriangle = sorted !! 0 + sorted !! 1 >= sorted !! 2
    cosangle1 = (r1'^2 + p1p2^2 - r2'^2) / (2 * r1' * p1p2)
    sinangle1 = sqrt (1 - cosangle1^2)
    vecUnit = signum (p2 - p1)
    center = p1 + scale vecUnit (r1' * cosangle1)

getPoints :: [Circle] -> Double -> [Point]
getPoints cs r = map fst cs ++ [ res
                               | (i,ci) <- zip [0..] cs
                               , (j,cj) <- zip [0..] cs
                               , i < j
                               , res <- encloseTwoCircle ci cj r
                               ]

canEnclose :: Circle -> Circle -> Bool
canEnclose (p1, r1) (p2, r2) = magnitude (p1 - p2) + r2 <= r1 + eps

bv2u :: [Bool] -> Int64
bv2u bv = foldl (\x y -> if y then shiftL x 1 + 1 else shiftL x 1) 0 bv

checkR :: [Circle] -> Double -> Bool
checkR cs r = or [ (i .|. j) == mask
                 | (r, i) <- zip [1..] bits
                 , j <- take r bits
                 ]
  where
    ps = getPoints cs r
    bits = Set.toList . Set.fromList $ [bv2u [canEnclose (p, r) c | c <- cs] | p <- ps]
    mask = shiftL 1 (length cs) - 1

bsearch :: (Double -> Bool) -> Double -> Double -> Double
bsearch check lo hi = (lo' + hi') / 2
  where
    go (lo, hi)
        | check mid = (lo, mid)
        | otherwise = (mid, hi)
      where
        mid = (lo + hi) / 2
    (lo', hi') = last $ takeWhile checkMargin $ take 200 $ iterate go (lo, hi)
    checkMargin (x, y) = x + eps < y

solve :: [Circle] -> Double
solve ps = bsearch (checkR ps) lowerBound upperBound
  where
    lowerBound = maximum $ map snd ps
    upperBound = (*3) . maximum $ [magnitude (pi - pj) + ri + rj | (pi, ri) <- ps, (pj, rj) <- ps]
