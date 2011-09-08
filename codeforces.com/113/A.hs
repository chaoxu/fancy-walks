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
    BS.words <$> readLine
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = putStr =<< solve . evalState parseInput <$> BS.getContents

solve words
    | any isNothing typesMaybe   = "NO"
    | length types == 1          = "YES"
    | any ((/=gender).snd) types = "NO"
    | checkStatement types'      = "YES"
    | otherwise                  = "NO"
  where
    typesMaybe = map getType words
    types = map fromJust typesMaybe
    gender = snd $ head types

    types' = map fst types

checkStatement :: [Words] -> Bool
checkStatement (Adjective:xs) = checkStatement xs
checkStatement (Noun:Verb:xs) = checkStatement (Noun:xs)
checkStatement [Noun] = True
checkStatement _ = False

data Words = Adjective | Noun | Verb deriving (Show, Eq)

getType :: ByteString -> Maybe (Words, Bool)
getType word
    | BS.pack "lios"   `BS.isSuffixOf` word = Just (Adjective, False)
    | BS.pack "liala"  `BS.isSuffixOf` word = Just (Adjective, True)
    | BS.pack "etr"    `BS.isSuffixOf` word = Just (Noun, False)
    | BS.pack "etra"   `BS.isSuffixOf` word = Just (Noun, True)
    | BS.pack "initis" `BS.isSuffixOf` word = Just (Verb, False)
    | BS.pack "inites" `BS.isSuffixOf` word = Just (Verb, True)
    | otherwise                             = Nothing

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
