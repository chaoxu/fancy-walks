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
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Graph

import Debug.Trace

parseInput = do
    n <- readInt
    a <- replicateM n readInt
    return (n, a)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = BS.putStr . myPrint =<< solve . evalState parseInput <$> BS.getContents

myPrint Nothing      = BS.pack "No solution"
myPrint (Just (x,y)) = BS.unlines [ BS.unwords (map (BS.pack.show) x)
                                  , BS.unwords (map (BS.pack.show) y)
                                  ]

check xs
    | length xs <= 2 = True
    | otherwise      = all (==d) ds
  where
    (d:ds) = zipWith (-) (tail xs) xs

solveCase a a0 a1
    | null lst2        = Nothing
    | not (check lst2) = Nothing
    | otherwise        = Just (lst1, lst2)
  where
    n = length a
    mapping = IntMap.fromList $ zip a [1..n]
    arr = listArray (1, n) a :: UArray Int Int

    index1' = map (mapping IntMap.!) $ takeWhile (`IntMap.member` mapping) [a0,a1..]

    ((x,y) : pairs) = zip index1' (tail index1')
    index1 = x : y : map snd (takeWhile (\(x', y') -> compare x' y' == compare x y) pairs)

    set = IntSet.fromList index1

    index2 = filter (`IntSet.notMember` set) [1..n]

    lst1 = map (arr!) (IntSet.toList set)
    lst2 = map (arr!) index2

solve (n, a)
    | n == 2    = Just ([a !! 0], [a !! 1])
    | otherwise = msum answer
  where
    h0 = a !! 0
    h1 = a !! 1
    h2 = a !! 2
    t0 = a !! (n - 1)
    t1 = a !! (n - 2)
    t2 = a !! (n - 3)
    answer = [ solveCase a h0 h1
             , solveCase a h0 h2
             , solveCase a h1 h2
             , solveCase a t0 t1
             , solveCase a t0 t2
             , solveCase a t1 t2
             ]
   

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
