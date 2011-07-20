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
import Control.DeepSeq
import Data.Tuple

import Debug.Trace

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        forbiddenEdges <- replicateM m ((,) <$> readInt <*> readInt)
        return (n, forbiddenEdges)
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

modulo :: Integral a => a
modulo = 9901 * 2

newtype ModP = ModP { unModP :: Int } deriving Eq

instance Num ModP where
    ModP a + ModP b = ModP $ (a + b) `mod` modulo
    ModP a - ModP b = ModP $ (a - b) `mod` modulo
    ModP a * ModP b = ModP $ (a * b) `mod` modulo
    signum = undefined
    abs = undefined
    fromInteger = ModP . fromInteger . (`mod` modulo)

instance NFData ModP where
    rnf (ModP a) = rnf a

instance Show ModP where
    show (ModP a) | even a = show (a `div` 2)
                  | odd a  = error "Show ModP: not a even number"

solve :: (Int, [(Int, Int)]) -> ModP
solve (n, forbiddens) = sum [ solveCase (n, subset) * signum
                            | subset <- subsequences forbiddens
                            , let signum = if even (length subset) then 1 else -1
                            ]

factorial :: Int -> ModP
factorial = (cache!)
  where
    bnds = (0,512)
    cache = listArray bnds lst :: Array Int ModP
    lst = scanl (*) 1 (map ModP $ tail $ range bnds)

power2 :: Int -> ModP
power2 = (cache!)
  where
    bnds = (0,512)
    cache = listArray bnds lst :: Array Int ModP
    lst = scanl (*) 1 (repeat 2)

solveCase :: (Int, [(Int, Int)]) -> ModP
solveCase (n, edges)
    | maximum degrees > 2               = 0
    | circles == 1 && n == length edges = 2
    | circles > 0                       = 0
    | otherwise                         = factorial (n - length edges - 1) * power2 (length [comp | comp <- comps, length comp > 1])
  where
    graph = buildG (1, n) $ edges ++ map swap edges
    degrees = elems $ outdegree graph

    comps = map F.toList $ components graph
    circles = length edges - sum (map (pred.length) comps)
