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
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Graph
import Control.Parallel.Strategies
import Debug.Trace

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        a <- replicateM m $ do
            num <- readInt
            replicateM num $ (,) <$> readInt <*> readInt
        return (n, m, a)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    input <- evalState parseInput <$> BS.getContents
    let output = parMap rdeepseq solve input
    forM_ (zip [1..] output) $ \(cas, result) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ result

solve :: (Int, Int, [[(Int,Int)]]) -> String
solve (flavors, users, a)
    | isNothing resList = "IMPOSSIBLE"
    | otherwise         = unwords . map show $ resArray
  where
    parseUser :: [(Int,Int)] -> (Maybe Int, [Int])
    parseUser prefers = (listToMaybe $ map fst malted, map fst unmalted)
      where
        (malted, unmalted) = partition (\(_,x) -> x == 1) prefers
    parsed = map parseUser a :: [(Maybe Int, [Int])]

    malts = listArray (1, users) $ map fst parsed
    edges = [ (user, flavor) | (user, (malt, unmalts)) <- zip [1..] parsed, flavor <- unmalts]
    swap (a, b) = (b, a)
    outDeg = accumArray (+) 0 (1, users) $ map (fmap (const 1)) edges
    inEdges = accumArray (flip (:)) [] (1, flavors) $ map swap edges

    init0 = nub [malts ! user | user <- [1..users], outDeg ! user == 0]
    resList = go (Seq.fromList init0) outDeg (Set.fromList init0)
    resArray = elems $ accumArray (+) 0 (1, flavors) [(flavor, 1) | flavor <- fromJust resList]

    go queue arr set = case Seq.viewl queue of
        EmptyL         -> Just []
        malted :< queue' -> if isNothing malted then Nothing else fmap (fromJust malted:) (go (queue' >< qappend) arr' set')
          where
            decreaseUnmalted = inEdges ! (fromJust malted)
            arr' = accum (+) arr $ zip decreaseUnmalted (repeat (-1))
            toAdd = Set.fromList [malts ! u | u <- decreaseUnmalted, arr' ! u == 0]
            added = Set.difference toAdd set
            set' = Set.union set added
            qappend = Seq.fromList . Set.toList $ added
