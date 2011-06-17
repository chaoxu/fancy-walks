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
    replicateM n (BS.unpack <$> readLine)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = putStr =<< solve . evalState parseInput <$> BS.getContents

data Widget = Widget { name :: String, width :: Integer, height :: Integer }
            | VBox { name :: String, border :: Integer, spacing :: Integer, contains :: [String] }
            | HBox { name :: String, border :: Integer, spacing :: Integer, contains :: [String] }

solve :: [String] -> String
solve seq = unlines $ map (show . calcWidget mapping) (Map.keys mapping)
  where
    mapping = execState (sequence_ $ map simulating seq) Map.empty

simulating :: String -> State (Map String Widget) ()
simulating ('V':'B':'o':'x':' ':name) = modify $ Map.insert name (VBox name 0 0 [])
simulating ('H':'B':'o':'x':' ':name) = modify $ Map.insert name (HBox name 0 0 [])
simulating ('W':'i':'d':'g':'e':'t':' ':def) = modify $ Map.insert name (Widget name x y)
  where
    (name, xy) = span (/='(') def
    (x, y) = read xy
simulating stmt
    | op == ".pack"        = modify $ Map.update (Just . addContains param) name 
    | op == ".set_border"  = modify $ Map.update (Just . setBorder (read param)) name 
    | op == ".set_spacing" = modify $ Map.update (Just . setSpacing (read param)) name 
  where
    (name, stmt2) = span (/='.') stmt
    (op, stmt3) = span (/='(') stmt2
    param = tail $ init stmt3

addContains chd (VBox name border spacing contains) = VBox name border spacing (chd:contains)
addContains chd (HBox name border spacing contains) = HBox name border spacing (chd:contains)

setSpacing spacing (VBox name border _ contains) = VBox name border spacing contains
setSpacing spacing (HBox name border _ contains) = HBox name border spacing contains

setBorder border (VBox name _ spacing contains) = VBox name border spacing contains
setBorder border (HBox name _ spacing contains) = HBox name border spacing contains

calcWidget mapping = (cache Map.!)
  where
    cache = Map.fromList [(names, goName names) | names <- Map.keys mapping]

    goName name = go (mapping Map.! name)
    go w@(Widget _ _ _) = w
    go (VBox name _ _ []) = Widget name 0 0
    go (HBox name _ _ []) = Widget name 0 0
    go (VBox name border spacing contains) = Widget name (maximum widths + border * 2) (sum heights + (len - 1) * spacing + border * 2)
      where
        widgets = map (calcWidget mapping) contains
        heights = map height widgets
        widths = map width widgets
        len = genericLength widgets

    go (HBox name border spacing contains) = Widget name (sum widths + (len - 1) * spacing + border * 2) (maximum heights + border * 2)
      where
        widgets = map (calcWidget mapping) contains
        heights = map height widgets
        widths = map width widgets
        len = genericLength widgets

instance Show Widget where
    show (Widget name x y) = name ++ " " ++ show x ++ " " ++ show y

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
