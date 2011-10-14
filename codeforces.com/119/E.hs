{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
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
--import Control.Monad.State
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

parseInput = do 
    n <- readInt
    m <- readInt
    points <- replicateM n (mkPoint3 <$> readDouble <*> readDouble <*> readDouble)
    planes <- replicateM m (mkPoint3 <$> readDouble <*> readDouble <*> readDouble)
    return (points, planes)
  where
    readDouble = read . BS.unpack <$> readString :: State ByteString Double
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = putStr =<< solve . evalState parseInput <$> BS.getContents

solve (points, planes) = unlines [show (solve2 (map trans points)) | plane <- planes, let trans = three2two plane]

myShuffle :: [a] -> [a]
myShuffle [] = []
myShuffle [a] = [a]
myShuffle a = myShuffle odds ++ myShuffle evens
  where
    odds = [ai | (i, ai) <- zip [0..] a, odd i]
    evens = [ai | (i, ai) <- zip [0..] a, even i]

solve2 :: [Point] -> Double
solve2 pts' = sqrt rad
  where
    pts = myShuffle pts'
    (center, rad) = execState monad ((0,0), 0)

    check :: Point -> State Circle Bool
    check pt = do
        ans <- get
        return (inCircle ans pt)

    monad :: State Circle ()
    monad = do
        forM_ (zip [0..] pts) $ \(i, pi) -> check pi >>= flip unless (do
            put (pi, 0)
            forM_ (zip [0..i-1] pts) $ \(j, pj) -> check pj >>= flip unless (do
                put (middle pi pj, dist pi pj / 4)
                forM_ (zip [0..j-1] pts) $ \(k, pk) -> check pk >>= flip unless (put $ mkCircle pi pj pk)
                )
            )

type Point = (Double, Double)

type Point3 = (Double, Double, Double)

type Circle = (Point, Double)

dist :: Point -> Point -> Double
dist (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2

inCircle :: Circle -> Point -> Bool
inCircle ((x1, y1), rsq) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2 <= rsq + eps

middle :: Point -> Point -> Point
middle (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

mkCircle :: Point -> Point -> Point -> Circle
mkCircle (x0, y0) (x1, y1) (x2, y2) = (pt, (dist pt (x0, y0) + dist pt (x1, y1) + dist pt (x2, y2)) / 3)
  where
    a1 = x1 - x0
    b1 = y1 - y0
    c1 = (a1^2+b1^2) / 2

    a2 = x2 - x0
    b2 = y2 - y0
    c2 = (a2^2+b2^2) / 2

    d = a1 * b2 - a2 * b1

    x = x0 + (c1 * b2 - c2 * b1) / d
    y = y0 + (a1 * c2 - a2 * c1) / d

    pt = (x, y)

{-sign :: Double -> Int
sign a | a < -eps  = -1
       | a > eps   = 1
       | otherwise = 0-}

det :: Point -> Point -> Point -> Double
det (x0, y0) (x1, y1) (x2, y2) = (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)

{-prop_mkCircle :: Point -> Point -> Point -> Bool
prop_mkCircle p0 p1 p2 = sign (det p0 p1 p2) == 0 || inCircle circle p0 && inCircle circle p1 && inCircle circle p2
  where
    circle = mkCircle p0 p1 p2-}

eps :: Double
eps = 1.0e-8

mkPoint3 :: Double -> Double -> Double -> Point3
mkPoint3 x y z = (x, y, z)

det3 :: Point3 -> Point3 -> Point3
det3 (x1, y1, z1) (x2, y2, z2) = (y1 * z2 - y2 * z1, z1 * x2 - z2 * x1, x1 * y2 - x2 * y1)

dot3 :: Point3 -> Point3 -> Double
dot3 (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

findOrg3 :: Point3 -> Point3
findOrg3 (x, y, z) | abs x > 0.5 = (y, -x, 0) -- input is integer
                   | otherwise   = (1, 0, 0)

unit3 :: Point3 -> Point3
unit3 (x, y, z) = let d = sqrt (x^2+y^2+z^2) in (x / d, y / d, z / d)

three2two :: Point3 -> Point3 -> Point
three2two vec p = (dot3 vecX p, dot3 vecY p)
  where
    vecX = unit3 (findOrg3 vec)
    vecY = unit3 (det3 vec vecX)

----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------

class (Monad m) => MonadState s m | m -> s where
	get :: m s
	put :: s -> m ()

modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do
	s <- get
	put (f s)

gets :: (MonadState s m) => (s -> a) -> m a
gets f = do
	s <- get
	return (f s)

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
	fmap f m = State $ \s -> let
		(a, s') = runState m s
		in (f a, s')

instance Applicative (State s) where
    pure = return
    (<*>) = ap

instance Monad (State s) where
	return a = State $ \s -> (a, s)
	m >>= k  = State $ \s -> let
		(a, s') = runState m s
		in runState (k a) s'

instance MonadState s (State s) where
	get   = State $ \s -> (s, s)
	put s = State $ \_ -> ((), s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = State $ f . runState m

withState :: (s -> s) -> State s a -> State s a
withState f m = State $ runState m . f

state = State
