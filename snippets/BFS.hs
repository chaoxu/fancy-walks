
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Hashable
import qualified Data.HashMap.Lazy as HM

bfs :: (Eq s, Hashable s) => s -> s -> (s -> [s]) -> Maybe [s]
bfs source target expand = go (HM.singleton source [source]) (Seq.singleton source)
  where
    go hashMap queue
        | Seq.null queue = Nothing
        | s == target    = Just paths
        | otherwise      = go hashMap' queue'
      where
        (s Seq.:< qtail) = Seq.viewl queue
        paths = fromJust $ HM.lookup s hashMap
        ts = filter (\k -> isNothing $ HM.lookup k hashMap) $ expand s
        hashMap' = foldl (\map t -> HM.insert t (t:paths) map) hashMap ts
        queue' = qtail Seq.>< Seq.fromList ts

