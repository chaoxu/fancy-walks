
import Control.Monad
import Data.Array

import Control.Monad.ST
import Data.Array.ST

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
type Expand gk gv k v = DGraph gk gv -> k -> v -> [(k, v)]

buildGraph :: Ix k => (k, k) -> [(k, (k, v))] -> DGraph k v
buildGraph bnds edges = accumArray (flip (:)) [] bnds edges

shortestPath :: (Ix gk, Ix k, Ord v, Bounded v) => DGraph gk gv -> Expand gk gv k v -> (k,k) -> k -> v -> Array k v
shortestPath graph expand bnds src sdis = runSTArray $ do
    dis <- newArray bnds maxBound
    writeArray dis src sdis
    dijkstra dis (unit sdis src)
    return dis
  where
    dijkstra dis heap | isEmpty heap = return ()
    dijkstra dis heap = do
        heapExpand <- filterM checkDis $ expand graph labelA disA
        dijkstra dis $ mergeAll (heap':map (\(k, v) -> unit v k) heapExpand)
      where
        (disA, labelA) = findMin heap
        heap' = deleteMin heap

        checkDis (k, v) = do
            v' <- readArray dis k
            when (v < v') $ writeArray dis k v
            return $ v < v'


defaultExpand :: (Ix k, Num v) => Expand k v k v
defaultExpand graph src sdis = map (\(k,v) -> (k, sdis + v)) (graph ! src)

