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
    limit <- (,) <$> readInteger <*> readInteger
    k <- readInt
    from <- (,) <$> readInteger <*> readInteger
    to <- (,) <$> readInteger <*> readInteger
    return (limit, k, from, to)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = print =<< solve . evalState parseInput <$> BS.getContents

add0 len str | length str > len = replicate len 'F'
add0 len str | length str < len = replicate (len - length str) '0' ++ str
add0 len str = str

solveCase (h, m) k (h', m') = searchH (0, False, False, 0)
  where
    hUpper = listArray (0, hLen - 1) $ map digitToInt $ show (h - 1) :: UArray Int Int
    mUpper = listArray (0, mLen - 1) $ map digitToInt $ show (m - 1) :: UArray Int Int

    hUpper' = listArray (0, hLen - 1) $ map digitToInt $ add0 hLen $ show h' :: UArray Int Int
    mUpper' = listArray (0, mLen - 1) $ map digitToInt $ add0 mLen $ show m' :: UArray Int Int

    hLen = length $ show (h - 1)
    mLen = length $ show (m - 1)

    hNonZero = length $ filter (/='0') $ show (h - 1)
    mNonZero = length $ filter (/='0') $ show (m - 1)

    searchH :: (Int, Bool, Bool, Int) -> Integer
    searchH = (cache!)
      where
        bnds = ((0, False, False, 0), (hLen, True, True, hLen))
        cache = listArray bnds $ map go $ range bnds :: Array (Int, Bool, Bool, Int) Integer

        go (pos, ltH, ltH', consec9) | pos < hLen = sum lst
          where
            lst = [ searchH (pos + 1, ltH || digit < (hUpper ! pos), ltH' || digit < (hUpper' ! pos), if digit == 9 then consec9 + 1 else 0)
                  | digit <- [0..9]
                  , ltH || digit <= (hUpper ! pos)
                  , ltH' || digit <= (hUpper' ! pos)
                  ]

        go (pos, ltH, ltH', consec9)
            | not ltH   = searchM (0, False, ltH', 0, hNonZero)
            | otherwise = searchM (0, False, ltH', 0, consec9 + 1)

    searchM :: (Int, Bool, Bool, Int, Int) -> Integer
    searchM = (cache!)
      where
        bnds = ((0, False, False, 0, 0), (mLen, True, True, mLen, hLen))
        cache = listArray bnds $ map go $ range bnds :: Array (Int, Bool, Bool, Int, Int) Integer

        go (pos, ltM, ltM', consec9, changeH) | pos < mLen = sum lst
          where
            lst = [ searchM (pos + 1, ltM || digit < (mUpper ! pos), ltM' || digit < (mUpper' ! pos), if digit == 9 then consec9 + 1 else 0, changeH)
                  | digit <- [0..9]
                  , ltM || digit <= (mUpper ! pos)
                  , ltM' || digit <= (mUpper' ! pos)
                  ]

        go (pos, ltM, ltM', consec9, changeH)
            | not ltM'  = 0
            | not ltM   = if mNonZero + changeH >= k then 1 else 0
            | otherwise = if consec9 + 1 >= k then 1 else 0

solve (limit, k, from, to)
    | from <= to = count to - count from
    | otherwise  = (count limit - count from) + count to
  where
    count = solveCase limit k

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
