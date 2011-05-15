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
import Text.Printf

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        return n
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

modulo :: Integral a => a
modulo = 1000

newtype Mod = Mod Int deriving Eq

instance Show Mod where
    show (Mod a) = printf "%03d" a

instance Num Mod where
    Mod a + Mod b = Mod $ (a + b) `mod` modulo
    Mod a - Mod b = Mod $ (a - b) `mod` modulo
    Mod a * Mod b = Mod $ (a * b) `mod` modulo
    fromInteger a = Mod $ fromInteger a `mod` modulo
    signum = undefined
    abs = undefined

newtype Matrix a = Matrix { unMatrix :: [[a]] } deriving (Show,Eq)

multMatrix :: (Num a) => Matrix a -> Matrix a -> Matrix a
multMatrix (Matrix a) (Matrix b) = Matrix [ [ sum $ zipWith (*) ra cb | cb <- b'] | ra <- a]
  where
    b' = transpose b

fastPower :: (Num a) => Matrix a -> Int -> Matrix a
fastPower ma 0 = Matrix [[if i == j then 1 else 0 | j <- [1..n]] | i <-  [1..n]]
  where
    n = length (unMatrix ma)
fastPower ma p
    | p `mod` 2 == 1 = multMatrix res2 ma
    | otherwise      = res2
  where
    res = fastPower ma (p `div` 2)
    res2 = multMatrix res res

transMatrix :: Matrix Mod
transMatrix = Matrix [[0, 1], [(-4), 6]]

initColumn :: Matrix Mod
initColumn = Matrix [[2], [6]]

solve n = show $ ans - 1
  where
    ans = head . head $ unMatrix (multMatrix ma initColumn)
    ma = fastPower transMatrix n
