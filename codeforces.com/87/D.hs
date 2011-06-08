{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, RankNTypes #-}
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

import Control.Monad.ST
import Data.Array.ST

import GHC.Conc (par, pseq)

data Edge = Edge { fromE :: Int
                 , destE :: Int
                 , costE :: Int
                 , label :: Int
                 } deriving (Show, Eq)

instance Ord Edge where
    compare = comparing costE

parseInput = do 
    n <- readInt
    es <- forM [1..n-1] $ \i -> Edge <$> readInt <*> readInt <*> readInt <*> return i
    return (n, es)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
  (ans, edgeLabels) <- solve . evalState parseInput <$> BS.getContents
  putStrLn $ show ans ++ " " ++ show (length edgeLabels)
  putStrLn $ unwords $ map show edgeLabels

type UnionFind s = STArray s Int (Int,Int)

findAnc :: UnionFind s -> Int -> ST s (Int, Int)
findAnc uf a = do
    (fa,fnum) <- readArray uf a
    if fa == -1
      then
        return (a,fnum)
      else do
        ret <- findAnc uf fa
        writeArray uf a ret
        return ret

mergeComp :: Int -> Int -> UnionFind s -> ST s Bool
mergeComp x y uf = do
    (fx, nx) <- findAnc uf x
    (fy, ny) <- findAnc uf y
    if fx == fy 
      then
        return False
      else do
        writeArray uf fy (fx, nx + ny)
        writeArray uf fx (-1, nx + ny)
        return True

solve :: (Int, [Edge]) -> (Int64, [Int])
solve (n, es) = (maxValue, maxList)
  where
    sorted = sort es
    grouped = groupBy ((==) `on` costE) sorted

    costLen = runSTUArray $ do
        uf <- newArray (1, n) (-1,1) :: ST s (UnionFind s)
        ans <- newArray (1, n-1) 0 :: ST s (STUArray s Int Int64)
        solving grouped uf ans
        return ans

    maxValue = maximum $ elems costLen
    maxList = [ i
              | i <- [1..n-1]
              , (costLen ! i) == maxValue
              ]

solving :: [[Edge]] -> UnionFind s -> STUArray s Int Int64 -> ST s ()
solving [] _ _ = return ()
solving (es:ess) uf ans = do 
    ndsComp <- zip nds <$> mapM (findAnc uf) nds
    let addList = solveTree ndsComp es
    forM_ addList $ \(eID, delta) -> do
        val <- readArray ans eID
        writeArray ans eID (val + delta)
    forM_ es $ \e -> mergeComp (fromE e) (destE e) uf
    solving ess uf ans
  where
    nds = map head . group . sort $ map fromE es ++ map destE es

(!!!) :: IntMap a -> Int -> a
(!!!) = (IntMap.!)

solveTree :: [(Int, (Int, Int))] -> [Edge] -> [(Int, Int64)]
solveTree ndsComp es = ans
  where
    comps = map head . group . sort $ map (fst.snd) ndsComp

    swap (a, b) = (b, a)

    bnds = (1, 100000)
    
    comp2size
        | cnum < 10000 = let cache = IntMap.fromList $ map snd ndsComp in (cache !!!)
        | otherwise    = let cache = array bnds $ map snd ndsComp :: UArray Int Int in (cache !)

    node2comp
        | cnum < 10000 = let cache = IntMap.fromList $ map (fmap fst) ndsComp in (cache !!!)
        | otherwise    = let cache = array bnds $ map (fmap fst) ndsComp :: UArray Int Int in (cache !)

    comp2id
        | cnum < 10000 = let cache = IntMap.fromList $ zip comps [1..] in (cache !!!)
        | otherwise    = let cache = array bnds $ zip comps [1..] :: UArray Int Int in (cache !)

    id2comp = let cache = listArray (1, cnum) comps :: UArray Int Int in (cache !)
    id2size = let cache = listArray (1, cnum) $ map comp2size comps :: UArray Int Int in (cache !)

    cnum = length comps

    edgesID = [ (ia, ib)
              | e <- es
              , let ia = comp2id . node2comp $ fromE e
              , let ib = comp2id . node2comp $ destE e
              ]

    adjList = accumArray (flip (:)) [] (1, cnum) $ edgesID ++ map swap edgesID :: Array Int [Int]

    (subTreeSize, subTreeColor) = runST $ do
        sz <- newArray (1, cnum) 0 :: forall s. ST s (STUArray s Int Int)
        color <- newArray (1, cnum) 0 :: forall s. ST s (STUArray s Int Int)
        forM_ [1..cnum] $ \i -> do
            val <- readArray color i
            when (val == 0) $ dfs color i sz i 0 >> return ()
        szFreezed <- unsafeFreeze sz
        colorFreezed <- unsafeFreeze color
        return (szFreezed :: UArray Int Int, colorFreezed :: UArray Int Int)
      where
        dfs color root sz now prev = do
            writeArray color now root
            lst <- forM (adjList ! now) $ \chd -> do
                if chd == prev
                  then return 0
                  else dfs color root sz chd now
            let ret = sum lst + nowCompSize
            writeArray sz now ret
            return ret
          where
            nowCompSize = id2size now

    ans = [ (label e, delta)
          | e <- es
          , let ia = comp2id . node2comp $ fromE e
          , let ib = comp2id . node2comp $ destE e
          , let sa = subTreeSize ! ia
          , let sb = subTreeSize ! ib
          , let minv = sa `min` sb
          , let maxv = subTreeSize ! (subTreeColor ! ia)
          , let delta = fromIntegral minv * fromIntegral (maxv - minv) * 2 :: Int64
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
