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
import Text.Printf

parseInput = do 
    a <- readInt
    b <- readInt
    l <- readInt
    return (fromIntegral a, fromIntegral b, fromIntegral l)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = putStr =<< solve . evalState parseInput <$> BS.getContents

solve :: (Double, Double, Double) -> String
solve (a, b, l)
    | ans < 0   = "My poor head =("
    | otherwise = printf "%.10f" ans
  where
    w = ternarySearch (calcW (a, b, l)) (0, pi / 2)

    eps = 1.0e-11
    lst = [b | l <= a + eps] ++ [a | l <= b + eps] ++ [w]

    ans = maximum lst `min` l
    
calcW (a, b, l) ang = (x1 * y2 - x2 * y1) / l
  where
    y = sin ang * l
    x = cos ang * l

    -- pointToLine (b, a) (0, y) (x, 0)
    x1 = 0 - b
    y1 = y - a

    x2 = x - b
    y2 = 0 - a

ternarySearch :: Ord a => (Double -> a) -> (Double, Double) -> a
ternarySearch func (lo, hi) = go 300 lo hi
  where
    go 0 lo hi = func ((lo + hi) / 2)
    go t lo hi
        | func mid1 < func mid2 = go (t-1) lo mid2
        | otherwise             = go (t-1) mid1 hi
      where
        mid1 = (lo + lo + hi) / 3
        mid2 = (lo + hi + hi) / 3

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
