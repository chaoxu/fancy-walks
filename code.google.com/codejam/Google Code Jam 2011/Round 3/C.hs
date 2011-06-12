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

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        grid <- replicateM n (BS.unpack <$> readString)
        return (n, m, grid)
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
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show result

pow2 0 = 1
pow2 n = pow2 (n-1) * 2 `mod` modulo
  where
    modulo = 1000003

solve :: (Int, Int, [String]) -> Int
solve (n, m, grid')
    | checkAnswer = pow2 cycles
    | otherwise   = 0
  where
    bnds = ((0, 0), (n-1, m-1))

    grid = listArray bnds [ele | row <- grid', ele <- row] :: UArray (Int, Int) Char

    getDirection '|' = (1, 0)
    getDirection '-' = (0, 1)
    getDirection '/' = (-1, 1)
    getDirection '\\' = (1, 1)

    es = [ (x1 * m + y1, x2 * m + y2)
         | (i, j) <- range bnds
         , let (dx, dy) = getDirection (grid ! (i, j))
         , let (x1, y1) = ((i + dx) `mod` n, (j + dy) `mod` m)
         , let (x2, y2) = ((i - dx) `mod` n, (j - dy) `mod` m)
         ]

    swap (a, b) = (b, a)

    g = buildG (0, n * m - 1) $ es ++ map swap es

    g' = deleteOrphans g

    checkAnswer = and $ map (\x -> x == 0 || x == 2) $ elems $ outdegree g'

    cycles = length (components g') - length [x | x <- elems $ outdegree g', x == 0]

deleteOrphans :: Graph -> Graph 
deleteOrphans g
    | isNothing z = g
    | otherwise   = deleteOrphans $ g // [(x, delete y (g ! x)), (y, delete x (g ! y))]
  where
    z = listToMaybe [ (x, head (g ! x)) | x <- range (bounds g), length (g ! x) == 1]
    Just (x, y) = z
