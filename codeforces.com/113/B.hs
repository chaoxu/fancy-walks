{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe
import Data.Char
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Int
import Data.Word
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
    t <- readString
    sBegin <- readString
    sEnd <- readString
    return (t, sBegin, sEnd)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = print =<< solve . evalState parseInput <$> BS.getContents

magic :: Word64
magic = 0xa2facedead -- wish not to be hacked

hash :: Word64 -> Char -> Word64
hash v ch = v * magic + fromIntegral (ord ch)

solve (t, sBegin, sEnd) = sum $ map solveLen [gap..len]
  where
    pBegin = BS.findSubstrings sBegin t
    pEnd = BS.findSubstrings sEnd t

    len = BS.length t
    lBegin = BS.length sBegin
    lEnd = BS.length sEnd

    gap = lBegin `max` lEnd

    vBegin = accumArray (||) False (0, len) [(p, True) | p <- pBegin] :: UArray Int Bool
    vEnd = accumArray (||) False (0, len) [(p + lEnd, True) | p <- pEnd] :: UArray Int Bool

    prefixHash = listArray (0, len) $ scanl hash 0 (BS.unpack t) :: UArray Int Word64

    solveLen l = length $ group $ sort hashes
      where
        magicl = magic ^ l
        hashes = [ (prefixHash ! (s + l)) - (prefixHash ! s) * magicl
                 | s <- [0..len - l]
                 , vBegin ! s && vEnd ! (s + l)
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
