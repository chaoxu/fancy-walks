{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe
import Data.Char
import Data.Array
import Data.Int
--import Control.Monad.State
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as BS

solve n a = 
    (answer, go2 (1,1))
  where
    (a', n') = if even n then (a, n) else (a ++ [0], n + 1)

    arr = listArray (1, n') a' :: Array Int Int
    bnds = ((1,1), (n', n'))
    cache = listArray bnds [go idx | idx <- range bnds] :: Array (Int,Int) Int

    answer = go (1, 1)

    go (x, e)
        | even x || x < 1 || x > n' = 0
        | x + 1 == n'               = max val1 val2
        | otherwise                 = ans1 `min` ans2 `min` ans3
      where
        go' idx = cache ! idx
        val1 = arr ! e
        val2 = arr ! (x + 1)
        val3 = arr ! (x + 2)
        ans1 = max val2 val3 + go' (x + 2, e)
        ans2 = max val1 val3 + go' (x + 2, x + 1)
        ans3 = max val1 val2 + go' (x + 2, x + 2)

    go2 (x, e)
        | even x || x < 1 || x > n' = error "impossible"
        | x + 1 == n'               = [(e, x + 1)]
        | ans1 == ans               = (x + 1, x + 2) : go2 (x + 2, e)
        | ans2 == ans               = (e, x + 2) : go2 (x + 2, x + 1)
        | ans3 == ans               = (e, x + 1) : go2 (x + 2, x + 2)
        | otherwise                 = error "impossible"
      where
        ans = go (x, e)
        val1 = arr ! e
        val2 = arr ! (x + 1)
        val3 = arr ! (x + 2)
        go' idx = cache ! idx
        ans1 = max val2 val3 + go' (x + 2, e)
        ans2 = max val1 val3 + go' (x + 2, x + 1)
        ans3 = max val1 val2 + go' (x + 2, x + 2)

parseInput = do 
    n <- readInt
    a <- replicateM n readInt
    return (n, a)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    (n, a) <- evalState parseInput <$> BS.getContents
    let (ans, pairs) = solve n a
    print ans
    forM_ pairs $ \(x, y) -> if x <= n && y <= n then putStrLn (show x ++ " " ++ show y) else print (min x y)

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
