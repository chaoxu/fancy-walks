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

import GHC.Conc (par, pseq)

parMap f xs
    | null hi   = map f xs
    | otherwise = loRes `par` (hiRes `pseq` (loRes ++ hiRes))
  where
    (lo, hi) = splitAt 128 xs
    loRes = map f lo
    hiRes = parMap f hi

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

main = putStr =<< solve . evalState parseInput <$> BS.getContents

solve (n, m, grid') = show mans ++ " " ++ show (length $ filter (==mans) answer) ++ "\n"
  where
    bnds = ((1,1),(n,m))
    grid = listArray bnds [ele | row <- grid', ele <- row] :: UArray (Int,Int) Char

    answer = parMap walkingGrid [idx | idx <- range bnds, let ch = grid ! idx, ch /= '.']
    walkingGrid = walking grid

    mans = maximum answer

getD 'U' = (-1, 0)
getD 'D' = (1, 0)
getD 'R' = (0, 1)
getD 'L' = (0, -1)

getI 'U' = 0
getI 'D' = 1
getI 'R' = 2
getI 'L' = 3

walking :: UArray (Int,Int) Char -> (Int, Int) -> Int
walking grid (sx, sy) = runST $ do
    arr <- thaw initialArr :: ST s (STUArray s (Int, Int) Int)
    go arr (index bndsGrid (sx, sy)) 0
  where
    bndsGrid = bounds grid
    bnds = ((0, 0), (rangeSize bndsGrid - 1, 3))

    initialArr = runSTUArray $ do
        arr <- newListArray bnds [ if inRange bndsGrid xy' then index bndsGrid xy' else (-1)
                                 | (x, y) <- range bndsGrid
                                 , ch <- "UDRL"
                                 , let (dx, dy) = getD ch
                                 , let xy' = (x + dx, y + dy)
                                 ] :: ST s (STUArray s (Int,Int) Int)
        forM_ (assocs grid') (\(place, ch) -> when (ch == '.') (removeNode arr place))
        return arr

    grid' = listArray (0, rangeSize bndsGrid - 1) $ elems grid :: UArray Int Char

    go arr (-1) steps = return steps
    go arr place steps = do
        removeNode arr place
        nextPlace <- readArray arr (place, dir)
        go arr nextPlace (steps + 1)
      where
        dir = getI $ grid' ! place

removeNode :: STUArray s (Int,Int) Int -> Int -> ST s ()
removeNode arr place = {-forM_ [0..3] $ \i -> do
    back <- readArray arr (place, i `xor` 1)
    when (back >= 0) $ do
        front <- readArray arr (place, i)
        writeArray arr (back, i) front-} do
    x0 <- readArray arr (place, 0)
    x1 <- readArray arr (place, 1)
    x2 <- readArray arr (place, 2)
    x3 <- readArray arr (place, 3)
    when (x0 >= 0) $ writeArray arr (x0, 1) x1
    when (x1 >= 0) $ writeArray arr (x1, 0) x0
    when (x2 >= 0) $ writeArray arr (x2, 3) x3
    when (x3 >= 0) $ writeArray arr (x3, 2) x2

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
