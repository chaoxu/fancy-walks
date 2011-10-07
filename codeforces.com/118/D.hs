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
    n1 <- readInt
    n2 <- readInt
    k1 <- readInt
    k2 <- readInt
    return (n1, n2, k1, k2)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = print =<< solve . evalState parseInput <$> BS.getContents

solve (n1, n2, k1, k2) = countWays (n1, n2, 0, True) `mod` (10^8)
  where
    countWays :: (Int, Int, Int, Bool) -> Integer
    countWays (_, _, c, True) | c > k1 = 0
    countWays (_, _, c, False) | c > k2 = 0
    countWays (a, b, c, flag) = cache ! (a, b, c, flag)
      where
        bnds = ((0, 0, 0, False), (100, 100, 10, True))
        cache = listArray bnds $ map go $ range bnds :: Array (Int, Int, Int, Bool) Integer

        go (0, 0, _, _) = 1
        go (a, b, c, flag) = goA + goB
          where
            goA | a == 0    = 0
                | otherwise = countWays (a - 1, b, c + 1, flag)
            goB | b == 0    = 0
                | otherwise = countWays (b - 1, a, 1, not flag)

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
