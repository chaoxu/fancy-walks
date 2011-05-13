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

data Point a = Point a a a deriving (Show,Eq)

instance Num a => Num (Point a) where
    Point x1 y1 z1 + Point x2 y2 z2 = Point (x1 + x2) (y1 + y2) (z1 + z2)
    negate (Point x y z) = Point (-x) (-y) (-z)
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger x = Point y y y where y = fromInteger x

distanceTo :: Floating a => Point a -> Point a -> a
distanceTo (Point x1 y1 z1) (Point x2 y2 z2) = sqrt ((x1-x2)^2+(y1-y2)^2+(z1-z2)^2)

scale :: Floating a => Point a -> a -> Point a
scale (Point x y z) d = Point (x*d) (y*d) (z*d)

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        arr <- replicateM n $ do
            a <- Point <$> readDouble <*> readDouble <*> readDouble 
            b <- Point <$> readDouble <*> readDouble <*> readDouble 
            return (a, b)
        return (n, arr)
  where
    readDouble = read . BS.unpack <$> readString :: State ByteString Double
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ (solve params)

solve (n, arr) = show ansV ++ " " ++ show ans
  where
    pos = (`scale` (1/fromIntegral n)) . sum $ map fst arr :: Point Double
    vel = (`scale` (1/fromIntegral n)) . sum $ map snd arr :: Point Double
    
    lst = iterate tsearch (0.0, 1.0e100)
    ans = fst (lst !! 1000) :: Double

    ansV = distanceTo 0 (scale vel ans + pos)

    tsearch :: (Double, Double) -> (Double, Double)
    tsearch (lo, hi)
        | val1 <= val2 + 1.0e-13 = (lo, mid2)
        | otherwise              = (mid1, hi)
      where
        mid1 = lo * 0.55 + hi * 0.45
        mid2 = lo * 0.45 + hi * 0.55
        val1 = distanceTo 0 (scale vel mid1 + pos)
        val2 = distanceTo 0 (scale vel mid2 + pos)
