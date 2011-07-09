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

import Debug.Trace

parseInput = do 
    t <- readInt
    k <- readInt
    querys <- replicateM t $ (,) <$> readInteger <*> readInteger
    return (k, querys)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = BS.putStr =<< solve . evalState parseInput <$> BS.getContents


solve (k, querys) = BS.unlines [ BS.pack . show $ solveCase k countWays query | query <- querys] 
  where
    countWays :: (Int,Int) -> ModP
    countWays = (cache!)
      where
        bnds = ((0,0),(1024,1024))
        cache = listArray bnds $ map go $ range bnds :: Array (Int,Int) ModP

        go (prev, 0) = ModP 1
        go (prev, left) = ans47 + ansOther
          where
            ans47 = if prev == 0 || prev > k then ModP 2 * countWays (1, left - 1) else ModP 0
            ansOther = ModP 8 * countWays (if prev == 0 then 0 else prev + 1, left - 1)

solveCase k countWays (lo, hi) = solveNumber k countWays (hi + 1) - solveNumber k countWays lo

-- calculate the answer for [0,n)
solveNumber k countWays n = fromInteger n - go str 0 (length str - 1)
  where
    str = show n

    go [] _ _ = ModP 0
    go (x:xs) prev left = sum lst + if x /= '4' && x /= '7' || prev > k || prev == 0 then  go xs (trans x prev) (left - 1) else 0
      where
        lst = [ countWays (trans i prev, left)
              | i <- ['0'..pred x]
              , i /= '4' && i /= '7' || prev > k || prev == 0
              ]

    trans '4' _ = 1
    trans '7' _ = 1
    trans _ 0   = 0
    trans _ prev = prev + 1

modulo :: Integral a => a
modulo = 1000000007

newtype ModP = ModP Integer deriving Eq

instance Show ModP where
    show (ModP n) = show n

instance Num ModP where
    ModP a + ModP b = ModP $ (a + b) `mod` modulo
    ModP a - ModP b = ModP $ (a - b) `mod` modulo
    ModP a * ModP b = ModP $ fromIntegral ((fromIntegral a * fromIntegral b :: Int64) `mod` modulo)
    abs = undefined
    signum = undefined
    fromInteger = ModP . fromInteger . (`mod` modulo)

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
