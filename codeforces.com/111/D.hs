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
    rows <- readInt
    cols <- readInt
    colors <- readInt
    return (rows, cols, colors)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = print =<< solve . evalState parseInput <$> BS.getContents

newtype ModP = ModP { unModP :: Int64 } deriving Eq

modulo :: Integral a => a
modulo = 10^9 + 7

modP :: Int -> ModP
modP n = ModP $ fromIntegral (n `mod` modulo)

instance Num ModP where
    ModP a + ModP b = ModP $ (a + b) `mod` modulo
    ModP a - ModP b = ModP $ (a - b) `mod` modulo
    ModP a * ModP b = ModP $ (a * b) `mod` modulo
    fromInteger = ModP . fromInteger . (`mod` modulo)
    abs = undefined
    signum = undefined

instance Show ModP where
    show (ModP a) = show a

combine :: (Int,Int) -> ModP
combine = ModP . (cache !)
  where
    start = [modP 1]
    go x = zipWith (+) (modP 0 : x) (x ++ [modP 0])

    cache = array ((0, 0), (2048, 2048)) [ ((i, j), unModP cij)
                                         | (i, ri) <- zip [0..2048] $ iterate go start
                                         , (j, cij) <- zip [0..] ri
                                         ] :: UArray (Int,Int) Int64

-- return combines(n,0..n)
combines :: Int -> [Integer]
combines n = go 0 1
  where
    go p cnp | p == n = [cnp]
    go p cnp = cnp : go (p + 1) (cnp * toInteger (n - p) `div` toInteger (p + 1))
    
solve (rows, cols, colors) 
    | cols == 1 = modP colors ^ rows
    | otherwise = sum [calculate k | k <- [1..rows]]
  where
    combineColors = listArray (0, 2048) $ map fromInteger $ combines colors :: Array Int ModP

    calculate k = answer
      where
        singleCol = go k --modP k^rows - modP (k-1)^rows * modP k + ..
          where
            go 0 = 0
            go p = modP p^rows * combine (k, p) - go (p - 1)

        answer = sum [ ans
                     | common <- [0..k]
                     , k * 2 - common <= colors
                     , let middle = modP common ^ ((cols - 2) * rows)
                     , let ans = singleCol^2 * middle * combineColors ! (k * 2 - common) * combine (k * 2 - common, k - common) * combine (k, common)
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
