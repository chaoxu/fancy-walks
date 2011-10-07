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
    n <- readInt
    k <- readInt
    str <- readString
    return (n, k, str)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = BS.putStr =<< solve . evalState parseInput <$> BS.getContents

mergeAdd :: [Int] -> [Int] -> [Int]
mergeAdd [] xs = xs
mergeAdd xs [] = xs
mergeAdd (x:xs) (y:ys) = (x+y) : mergeAdd xs ys

getNum :: [Int] -> [Int] -> Int -> Int
getNum as xs limit | sum xs + maximum as < limit = maxBound
getNum as xs limit = minimum [ sum $ zipWith (*) lst [0..]
                             | (i, ai, xi) <- zip3 [0..] as xs
                             , let ys = (ai + xi) : mergeAdd (reverse $ take i xs) (drop (i + 1) xs)
                             , sum ys >= limit
                             , let zs = takeWhile (<limit) $ scanl (+) 0 ys
                             , let lst = zipWith (\y z -> min limit (y + z) - z) ys zs
                             ]

solve (n, k, str) = BS.unlines [ BS.pack (show cost)
                               , BS.pack (go 0 cost (replicate 10 0) lst "")
                               ]
  where
    len = BS.length str
    lst = [BS.count digit str | digit <- ['0'..'9']]

    cost = getNum (replicate 10 0) lst k

    go pos c prev next ans | pos == len = reverse ans
    go pos c prev next ans = head lst
      where
        ch = digitToInt $ BS.index str pos
        nnext = [if i == ch then xi - 1 else xi | (i, xi) <- zip [0..] next]

        lst = [ go (pos + 1) nc nprev nnext (intToDigit ch' : ans)
              | ch' <- [0..9]
              , let nprev = [if i == ch' then xi + 1 else xi | (i, xi) <- zip [0..] prev]
              , let nc = getNum nprev nnext k
              , nc + abs (ch - ch') == c
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
