
import Data.Maybe
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq

-- cabal install unordered-containers
import Data.Hashable
import qualified Data.HashMap.Lazy as HM

bfs :: (Eq s, Hashable s) => s -> (s -> [s]) -> s -> Maybe [s]
bfs source expand target = HM.lookup target pathMap
  where
    pathMap = go (HM.singleton source [source]) (Seq.singleton source)
    go hashMap queue
        | Seq.null queue = hashMap
        | otherwise      = go hashMap' queue'
      where
        (s :< qtail) = Seq.viewl queue
        paths = fromJust $ HM.lookup s hashMap
        ts = filter (\k -> isNothing $ HM.lookup k hashMap) $ expand s
        hashMap' = foldl (\map t -> HM.insert t (t:paths) map) hashMap ts
        queue' = qtail >< Seq.fromList ts

