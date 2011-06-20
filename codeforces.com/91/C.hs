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
import Data.STRef

parseInput = do 
    n <- readInt
    m <- readInt
    edges <- replicateM m ((,) <$> readInt <*> readInt)
    return (n, edges)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = BS.putStr =<< solve . evalState parseInput <$> BS.getContents

modulo :: Int
modulo = 1000000009

newtype ModP = ModP Int deriving Eq

instance Num ModP where
    ModP a + ModP b = ModP $ (a + b) `mod` modulo
    ModP a - ModP b = ModP $ (a - b) `mod` modulo
    ModP a * ModP b = ModP $ (a * b) `mod` modulo
    fromInteger = ModP . fromInteger
    signum = undefined
    abs = undefined

instance Show ModP where
    show (ModP a) = show a

solve :: (Int, [(Int, Int)]) -> ByteString
solve (n, edges) = BS.unlines . map (BS.pack . show . (\x -> x-1)) $ runST stMonad
  where
    stMonad :: ST s [ModP]
    stMonad = do
        uf <- buildUF (1, n) :: ST s (UnionFind s)
        eqs <- newSTRef 1 :: ST s (STRef s ModP)
        mapM (solveEdge uf eqs) edges

solveEdge :: UnionFind s -> STRef s ModP -> (Int, Int) -> ST s ModP
solveEdge uf eqs (a, b) = do
    merged <- mergeUF uf a b
    unless merged $ modifySTRef eqs (*2)
    readSTRef eqs

type UnionFind s = STUArray s Int Int

buildUF :: (Int, Int) -> ST s (UnionFind s)
buildUF bnds = newArray bnds (-1)

findUF :: UnionFind s -> Int -> ST s Int
findUF uf a = do
    fa <- readArray uf a
    if fa == -1
      then return a
      else do
        ret <- findUF uf fa
        writeArray uf a ret
        return ret

mergeUF :: UnionFind s -> Int -> Int -> ST s Bool
mergeUF uf a b = do
    fa <- findUF uf a
    fb <- findUF uf b
    if fa == fb
      then return False
      else do
        writeArray uf fa fb
        return True

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


