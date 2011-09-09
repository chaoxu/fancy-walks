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
    n <- readInt
    m <- readInt
    a <- readInt
    b <- readInt
    edges <- replicateM m ((,) <$> readInt <*> readInt)
    prob <- replicateM n readDouble
    return (n, m, a, b, edges, prob)
  where
    readDouble = read . BS.unpack <$> readString :: State ByteString Double
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = BS.putStr =<< solve . evalState parseInput <$> BS.getContents

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

solve (n, m, a, b, edges, prob) = BS.unlines $ map (BS.pack . show) answer
  where
    graph = accumArray (flip (:)) [] (1, n) $ edges ++ map swap edges :: Array Int [Int]
    p = listArray (1, n) prob :: UArray Int Double
    degree = fmap length graph

    source = stateToInt (max a b, min a b)

    states = [(i, j) | i <- [1..n], j <- [1..i]]
    stateToInt (a, b) = a * (a - 1) `div` 2 + (b - 1)

    numStates = length states

    oneOne n k = [if i == k then 1 else 0 | i <- [0..n-1]]

    matrix = map move states

    answer = gauss matrix !! source
    
    move (a, b)
        | a == b    = oneOne numStates id ++ oneOne n (a - 1)
        | otherwise = elems row ++ replicate n 0
      where
        id = stateToInt (a, b)
        row = accumArray (+) 0 (0, numStates - 1) ((id,-1):lst) :: UArray Int Double

        lst = [ (stateToInt (max i j, min i j), pi * pj)
              | let (stayA, moveA) = (p ! a, (1 - p ! a) / fromIntegral (degree ! a))
              , let (stayB, moveB) = (p ! b, (1 - p ! b) / fromIntegral (degree ! b))
              , (i, pi) <- (a, stayA) : zip (graph ! a) (repeat moveA)
              , (j, pj) <- (b, stayB) : zip (graph ! b) (repeat moveB)
              ]

-- can't handle free variables or extra equations
gauss :: (Ord a, Fractional a) => [[a]] -> [[a]]
gauss ma = go [] ma
  where
    go xs [] = reverse xs
    go xs ys = go (row' : map handle xs) (map handle (prefix ++ suffix))
      where
        pivot = fst $ maximumBy (comparing (abs.head.snd)) $ zip [0..] ys
        (prefix, (value:row):suffix) = splitAt pivot ys
        row' = map (/value) row
        handle (r:rs) = zipWith (\x y -> x - y * r) rs row'

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
