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
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Graph
import Control.Parallel.Strategies
import Debug.Trace

-- cabal install unordered-containers
import Data.Hashable
import qualified Data.HashMap.Lazy as HM

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        es <- replicateM m ((,) <$> readInt <*> readInt)
        return (n, es)
  where
    readInt = read . BS.unpack <$> readString :: State ByteString Int
    readString = state $ BS.span (not . isSpace') . BS.dropWhile isSpace'
    isSpace' ch = isSpace ch || ch == ','

main = do
    input <- evalState parseInput <$> BS.getContents
    let output = parMap rdeepseq solve input
    forM_ (zip [1..] output) $ \(cas, result) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show' result
  where
    show' (a, b) = show a ++ " " ++ show b

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

bfs :: (Eq s, Hashable s) => s -> (s -> [s]) -> s -> Maybe Int
bfs source expand target = HM.lookup target pathMap
  where
    pathMap = go (HM.singleton source 0) (Seq.singleton source)
    go hashMap queue
        | Seq.null queue = hashMap
        | otherwise      = go hashMap' queue'
      where
        (s :< qtail) = Seq.viewl queue
        pathsLen = fromJust $ HM.lookup s hashMap
        ts = filter (\k -> isNothing $ HM.lookup k hashMap) $ expand s
        hashMap' = foldl (\map t -> HM.insert t (pathsLen + 1) map) hashMap ts
        queue' = qtail >< Seq.fromList ts

solve :: (Int, [(Int,Int)]) -> (Int, Int)
solve (n, es) = (distFromSource sink - 1, getNear (sink, sink) + 1)
  where
    bnds = (0, n-1)
    es2 = es ++ map swap es
    edgeList = accumArray (flip (:)) [] bnds es2 :: Array Int [Int]

    isConnected = (cache!)
      where
        cache = accumArray (||) False ((0,0),(n-1,n-1)) $ [(idx, True) | idx <- es2] :: UArray (Int,Int) Bool

    source = 0
    sink = 1
    inf = 10^9 :: Int
    distFromSource = fromMaybe inf . bfs source (edgeList !)

    getNear :: (Int,Int) -> Int
    getNear = (cache!)
      where
        bnds = ((0,0),(n-1,n-1))
        cache = listArray bnds $ map go $ range bnds :: Array (Int,Int) Int

        go (prev, now) | now == source = 0
        go (prev, now) = maximum answers
          where
            answers = [ calcCost (prev, now, next) + getNear (now, next)
                      | next <- edgeList ! now
                      , distFromSource next + 1 == distFromSource now
                      ]

        calcCost (prev2, prev1, now) = length answers - if now == source then 0 else 1
          where
            answers = [ next
                      | next <- edgeList ! now
                      , next /= prev1
                      , not (prev2 /= sink && isConnected (next, prev2))
                      , not (prev1 /= sink && isConnected (next, prev1))
                      ]
