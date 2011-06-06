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
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
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
        u <- replicateM m readInt
        v <- replicateM m readInt
        return (n, zip u v)
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
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show' result
  where
    show' (a, b) = show a ++ "\n" ++ unwords (map show b)

splitComp :: [(Int,Int)] -> [Int] -> [[Int]]
splitComp es seq
    | isNothing sp = [seq]
    | otherwise    = concatMap (splitComp $ delete (x, y) es) [spL, spR]
  where
    set = IntSet.fromList seq
    
    sps = [ (x, y)
          | (x, y) <- es
          , IntSet.member x set
          , IntSet.member y set
          ]

    sp = listToMaybe sps

    (x, y) = fromJust sp
    (lo, hi) = break (flip elem [x,y]) seq

    seq' = hi ++ lo
    
    h1 = head seq'
    (ker1, h2:ker2) = break (flip elem [x,y]) $ tail seq'

    spL = [h1] ++ ker1 ++ [h2]
    spR = [h2] ++ ker2 ++ [h1]

(!!!) :: IntMap a -> Int -> a
(!!!) = (IntMap.!)

colorComp :: [([Int], [(Int,Int)])] -> ([Int], [(Int,Int)]) -> Int -> Maybe ((Int,Int), (Int,Int)) -> IntMap Int
colorComp splitedWithEdges (seq, edges) colors prevMap = foldl IntMap.union nowMap adjacentComp
  where
    ((x,y),(cx,cy)) = fromJust prevMap

    seqWithColor
        | isNothing prevMap = zip seq $ colors' ++ cycle (tail colors')
        | otherwise         = zip (x':y':seq') $ mapX x':mapX y':(cycle colors')
      where
        colors' = if isNothing prevMap then [1..colors] else ([1..colors] \\ [cx,cy]) ++ [mapX y']

        (lo, hi) = break (flip elem [(x,y),(y,x)]) (zip seq (tail seq ++ [head seq]))
        x':y':seq' = map fst $ hi ++ lo
        
        mapX a | a == x = cx
        mapX a | a == y = cy

    nowMap = IntMap.fromList seqWithColor

    edges' = if isNothing prevMap then edges else edges \\ [(x,y),(y,x)]
    
    adjacentComp = [ colorComp splitedWithEdges (tseq, tedges) colors (Just ((u,v),(nowMap !!! u, nowMap !!! v)))
                   | (u, v) <- edges'
                   , (tseq, tedges) <- splitedWithEdges
                   , tseq /= seq
                   , elem (u,v) tedges || elem (v,u) tedges
                   ]

getCrossingEdges :: Int -> [Int] -> [(Int,Int)] 
getCrossingEdges n a = filter (\(x, y) -> not $ elem (abs (x - y)) [1, n-1]) es
  where
    es = zip a (tail a ++ [head a])

solve :: (Int, [(Int,Int)]) -> (Int, [Int])
solve (n, es) 
    | not checkAnswer = error $ show (n, es, splited, allColorMap)
    | otherwise       = (colors, map snd $ IntMap.assocs allColorMap)
  where
    splited = splitComp es [1..n]
    colors = minimum $ map length splited
    splitedWithEdges = zip splited $ map (getCrossingEdges n) splited

    allColorMap = colorComp (tail splitedWithEdges) (head splitedWithEdges) colors Nothing

    checkAnswer = and [ length (group $ sort color) == colors
                      | sp <- splited
                      , let color = map (allColorMap !!!) sp
                      ]
