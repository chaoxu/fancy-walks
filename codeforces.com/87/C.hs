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
    return n
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = print =<< solve . evalState parseInput <$> BS.getContents

generate :: Int -> [(Int, Int)]
generate sumLimit = concatMap generateGivingNum $ takeWhile (\x -> x * (x + 1) <= sumLimit * 2) [2..]
  where
    generateGivingNum n = [ (fstNum, n)
                          | fstNum <- [1..(sumLimit - sum0) `div` n]
                          , fstNum * n + sum0 <= sumLimit
                          ]
      where
        sum0 = (0 + n - 1) * n `div` 2

solve :: Int -> Int
solve n
    | solveGame n == 0 = -1
    | otherwise        = minimum $ map snd strategiesOkay
  where
    strategiesN = arr ! n
    strategiesOkay = filter ((==0).solveStrategies) strategiesN

    bnds = (1, n)
    
    arr = accumArray (flip (:)) [] bnds [ (sum, (fstNum, num))
                                        | (fstNum, num) <- generate n
                                        , let sum = (fstNum + fstNum + num - 1) * num `div` 2
                                        ] :: Array Int [(Int,Int)]

    solveGameXor :: Int -> Int
    solveGameXor = (cache!)
      where
        bnds = (0, n)
        cache = listArray bnds $ map go $ range bnds :: Array Int Int

        go 0 = 0
        go x = solveGameXor (x - 1) `xor` solveGame x

    solveGame :: Int -> Int
    solveGame = (cache!)
      where
        bnds = (1, n)
        cache = listArray bnds $ map go $ range bnds :: Array Int Int

        go x
            | null strategies = 0 -- Lose the game
            | otherwise       = mex $ map solveStrategies strategies
          where
            strategies = arr ! x

    solveStrategies :: (Int, Int) -> Int
    solveStrategies (fstNum, num) = solveGameXor (fstNum - 1) `xor` solveGameXor lstNum
      where
        lstNum = fstNum + num - 1

    mex :: [Int] -> Int
    mex xs = fst . head $ filter (\(x,y) -> x /= y) $ zip [0..] (sorted ++ [-1])
      where
        sorted = IntSet.toList $ IntSet.fromList xs

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
