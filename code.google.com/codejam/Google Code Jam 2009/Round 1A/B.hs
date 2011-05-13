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
import Control.Monad.State
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
import Control.Monad.ST
import Data.Array.ST
import Debug.Trace

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        lights <- replicateM n $ replicateM m $ do
            ns <- readInt64
            we <- readInt64
            time <- readInt64
            return (ns, we, time)
        return (n, m, lights)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    readInt64 = readInt >>= \x -> return (fromIntegral x :: Int64)
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve params)

data Ord k => PairingHeap k v = Empty | Heap k v [PairingHeap k v]

empty :: Ord k => PairingHeap k v
empty = Empty

isEmpty :: Ord k => PairingHeap k v -> Bool
isEmpty Empty = True
isEmpty _ = False

unit :: Ord k => k -> v -> PairingHeap k v
unit k v = Heap k v []

insert :: Ord k => k -> v -> PairingHeap k v -> PairingHeap k v
insert k v heap = merge (unit k v) heap

merge :: Ord k => PairingHeap k v -> PairingHeap k v -> PairingHeap k v
merge Empty a = a
merge a Empty = a
merge h1@(Heap k1 v1 hs1) h2@(Heap k2 v2 hs2) =
    if k1 < k2 then
        Heap k1 v1 (h2:hs1)
      else
        Heap k2 v2 (h1:hs2)

mergeAll :: Ord k => [PairingHeap k v] -> PairingHeap k v
mergeAll [] = Empty
mergeAll [h] = h
mergeAll (x:y:zs) = merge (merge x y) (mergeAll zs)

findMin :: Ord k => PairingHeap k v -> (k, v)
findMin Empty = error "findMin: empty heap"
findMin (Heap k v _) = (k, v)

deleteMin :: Ord k => PairingHeap k v -> PairingHeap k v
deleteMin Empty = Empty
deleteMin (Heap _ _ hs) = mergeAll hs

hToList Empty = []
hToList (Heap k v hs) = (k, v) : hToList (mergeAll hs)

type DGraph k v = Array k [(k,v)]
type Expand k gv v = DGraph k gv -> k -> v -> [(k, v)]

buildGraph :: Ix k => (k, k) -> [(k, (k, v))] -> DGraph k v
buildGraph bnds edges = accumArray (flip (:)) [] bnds edges

shortestPath :: (Ix k, Ord v, Bounded v) => DGraph k gv -> Expand k gv v -> k -> v -> Array k v
shortestPath graph expand src sdis = runSTArray $ do
    dis <- newArray (bounds graph) maxBound
    dijkstra dis (unit sdis src)
    return dis
  where
    dijkstra dis heap | isEmpty heap = return ()
    dijkstra dis heap = do
        disA' <- readArray dis labelA
        if disA' <= disA then
            dijkstra dis heap'
          else do
            writeArray dis labelA disA
            dijkstra dis $ mergeAll (heap':heapExpand)
      where
        (disA, labelA) = findMin heap
        heap' = deleteMin heap
        heapExpand = map (\(k, v) -> unit v k) $ expand graph labelA disA

defaultExpand :: (Ix k, Num v) => Expand k v v
defaultExpand graph src sdis = map (\(k,v) -> (k, sdis + v)) (graph ! src)

solve (n, m, lights') = result
  where
    lights = listArray ((0,0),(n-1,m-1)) [ele | row <- lights', ele <- row]
    bnds = ((0,0), (n*2-1,m*2-1))

    calcBlock (x, y) = (x `div` 2, y `div` 2)
    calcDelay (bx, by) isNS 
        | isNS      = Just (period, time `mod` period, ns)
        | otherwise = Just (period, (time + ns) `mod` period, we)
      where
        (ns, we, time) = lights ! (bx, by)
        period = ns + we

    myExpand graph src stime = map go (graph ! src)
      where
        go (dest, (cost, Nothing)) = (dest, stime + cost)
        go (dest, (cost, Just (period, start, during)))
            | passed < during = (dest, stime + cost)
            | otherwise       = (dest, stime' + cost)
          where
            now = stime `mod` period
            passed = (now - start + period) `mod` period
            stime' = stime - passed + period

    edges = [ (idx, (target, (cost, delay)))
            | idx <- range bnds
            , (dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)]
            , let block = calcBlock idx
            , let target = (fst idx+dx,snd idx+dy)
            , inRange bnds target
            , let inSameBlock = block == calcBlock target
            , let cost = if inSameBlock then 1 else 2
            , let delay = if inSameBlock then calcDelay block (dx/=0) else Nothing
            ]

    graph = buildGraph bnds edges

    source = (n*2-1, 0)
    target = (0, m*2-1)

    distance = shortestPath graph myExpand source 0
    result = distance ! target
