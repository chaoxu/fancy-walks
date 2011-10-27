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
    n <- readInteger
    k <- readInteger
    return (n, k)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

lucky digits = [sum subset * 3 + ones | subset <- subsequences masks]
  where
    ones = (10^digits-1) `div` 9 * 4
    masks = map (10^) [0..digits-1]

luckys = sort $ concat [lucky digits | digits <- [1..10]]

main = print =<< solve . evalState parseInput <$> BS.getContents

factorial :: Integer -> Integer
factorial n = cache !! fromIntegral n
  where
    cache = scanl (*) 1 [1..]

findPermutation :: Integer -> Integer -> Maybe [Integer]
findPermutation n k
    | k >= factorial n = Nothing
    | n == 0           = Just []
    | isNothing perm   = Nothing
    | otherwise        = Just $ now : [if x >= now then x + 1 else x | x <- fromJust perm]
  where
    (now, nk) = k `divMod` (factorial (n-1))
    perm = findPermutation (n - 1) nk

isLucky :: Integer -> Bool
isLucky n = all (`elem` "47") $ show n

solve (n, k) | n >= 20 = prefixCount + suffixCount
  where
    lst20 = findPermutation 20 (k-1)
    prefixCount = length [x | x <- luckys, x <= n - 20]
    suffixCount = length [ i
                         | (i, pi) <- zip [0..] (fromJust lst20)
                         , isLucky (i + n - 19)
                         , isLucky (pi + n - 19)
                         ]

solve (n, k)
    | isNothing lst = -1
    | otherwise     = count
  where
    lst = findPermutation n (k-1)
    count = length [ i
                   | (i, pi) <- zip [0..] (fromJust lst)
                   , isLucky (i + 1)
                   , isLucky (pi + 1)
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
