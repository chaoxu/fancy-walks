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
    prange <- (,) <$> readInteger <*> readInteger
    vrange <- (,) <$> readInteger <*> readInteger
    k <- readInt
    return (prange, vrange, k)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = print =<< solve . evalState parseInput <$> BS.getContents

lucky = sort $ concat [ map read lst :: [Integer]
                      | digits <- [1..9]
                      , let lst = replicateM digits "47"
                      ] ++ [-inf, inf]

inf = 10^20

luckyL = zip (map succ lucky) (tail lucky)
luckyR = zip lucky (map pred (tail lucky))

solve (prange, vrange, k) = fromIntegral hits / fromIntegral all :: Double
  where
    hits = sum $ zipWith (solveCase prange vrange) luckyL (drop k luckyR)
    all = countNum prange * countNum vrange

mergeInt (l1, r1) (l2, r2) = (max l1 l2, min r1 r2)
countNum (l, r) | l > r     = 0
                | otherwise = r - l + 1

solveCase prange vrange int1 int2 = num1 + num2 - num3
  where
    num1 = countNum (mergeInt prange int1) * countNum (mergeInt vrange int2)
    num2 = countNum (mergeInt prange int2) * countNum (mergeInt vrange int1)

    int' = mergeInt int1 int2
    num3 = countNum (mergeInt prange int') * countNum (mergeInt vrange int')

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
