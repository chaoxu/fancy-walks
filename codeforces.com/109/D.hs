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

import Control.Monad.ST
import Data.Array.ST

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

main = BS.putStr =<< solve . evalState parseInput <$> BS.getContents

is47 n | n < 10                = n == 4 || n == 7
       | elem lastDigit [4, 7] = is47 n'
       | otherwise             = False
  where
    (n', lastDigit) = n `divMod` 10

isSorted xs = and $ zipWith (<=) xs (tail xs)

solve (n, a)
    | null pos47 = BS.pack $ if isSorted a then "0" else "-1"
    | otherwise  = BS.unlines (BS.pack (show $ length answer) : [ BS.pack $ show a ++ " " ++ show b | (a, b) <- answer])
  where
    arr = listArray (1, n) a :: UArray Int Int
    s = sort $ zip a [1..]
    from = listArray (1, n) (map snd s) :: UArray Int Int

    pos47 = [i | (i, ai) <- zip [1..] a, is47 ai]

    decompress = runST $ do
        vis <- newArray (1, n) False :: ST s (STUArray s Int Bool)
        filter (not.null) <$> mapM (dfs vis []) [1..n]
      where
        dfs :: STUArray s Int Bool -> [Int] -> Int -> ST s [Int]
        dfs vis lst p = do
            vp <- readArray vis p
            if vp 
              then return lst
              else do
                writeArray vis p True
                dfs vis (p : lst) (from ! p)

    target = head pos47

    dep = b ++ a
      where
        (a, b) = partition (elem target) decompress
    
    walkCycle cycle = reverse (zip (last cycle : init cycle) cycle)

    solveCase [] = []
    solveCase [x] = []
    solveCase cycle | elem target cycle = init (walkCycle cycle')
      where
        pos = fromJust $ elemIndex target cycle
        cycle' = drop (pos + 1) cycle ++ take (pos + 1) cycle
    solveCase cycle = [(target, last cycle)] ++ init (walkCycle cycle) ++ [(target, head cycle)]
    
    answer = concatMap solveCase dep

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
