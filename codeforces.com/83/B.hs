{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe
import Data.Char
import Data.Array
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
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Graph

parseInput = do 
    n <- readInt
    k <- fromIntegral <$> readInteger
    a <- map fromIntegral <$> replicateM n readInt
    return (n, k, a)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    (n, k, a) <- evalState parseInput <$> BS.getContents
    let s = sum a
    when (k > s) $ putStrLn "-1"
    when (k < s) $ putStrLn $ unwords . map show $ solve n k a

solve :: Int -> Int64 -> [Int64] -> [Int]
solve n k a = go pairs 0 k
  where
    intMap = foldl (\map v -> IntMap.insertWith (+) (fromIntegral v) 1 map) IntMap.empty a :: IntMap Int64
    ass = IntMap.assocs intMap
    pairs = scanr (\(k, v) (_,v') -> (fromIntegral k, v+v')) (maxBound,0) ass :: [(Int64, Int64)]

    go :: [(Int64,Int64)] -> Int64 -> Int64 -> [Int]
    go [] _ _ = []
    go ((k, v):xs) k' num
        | num >= tot = go xs k (num - tot)
        | otherwise  = map fst $ hi ++ lo'
      where
        tot = (k - k') * v
        a' = filter (\(id, num) -> num >= k) $ zip [1..] a
        (height, width) = num `divMod` fromIntegral (length a')
        (lo, hi) = splitAt (fromIntegral width) a'
        lo' = if height + 1 == k - k' then filter (\(id, num) -> num > k) lo else lo

--{{{ Start of a minimal State Monad
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
--}}} end of a minimal State Monad
