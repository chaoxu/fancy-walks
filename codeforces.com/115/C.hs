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
    grid <- replicateM n (BS.unpack <$> readString)
    return (n, m, grid)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = print =<< solve . evalState parseInput <$> BS.getContents

modulo = 1000003

newtype ModP = ModP Integer deriving Eq

instance Show ModP where
    show (ModP a) = show a

instance Num ModP where
    ModP a + ModP b = ModP $ (a + b) `mod` modulo
    ModP a - ModP b = ModP $ (a - b) `mod` modulo
    ModP a * ModP b = ModP $ (a * b) `mod` modulo
    signum = undefined
    abs = undefined
    fromInteger = ModP . (`mod` modulo)

solve (n, m, grid)
    | isConflict = 0
    | otherwise  = fromInteger 2 ^ frees :: ModP
  where
    lst = concat [ map (process (i, j)) (getD cij)
                 | (i, row) <- zip [0..] grid
                 , (j, cij) <- zip [0..] row
                 ]

    process (x, y) L = (x, even y)
    process (x, y) R = (x, odd y)
    process (x, y) U = (n + y, even x)
    process (x, y) D = (n + y, odd x)

    arr = accumArray merge None (0, n + m - 1) lst :: Array Int Info

    answer = elems arr

    isConflict = any (==Conflict) answer
    frees = length $ filter (==None) answer

    merge (Unique bool) b
        | bool == b = Unique bool
        | otherwise = Conflict

    merge Conflict _ = Conflict
    merge None x = Unique x

data Direction = L | R | U | D deriving (Show, Eq)

data Info = None
          | Unique Bool
          | Conflict deriving (Show, Eq)

getD '1' = [L, U]
getD '2' = [L, D]
getD '3' = [R, D]
getD '4' = [R, U]
getD _   = []

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
