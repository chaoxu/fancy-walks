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
import Control.Parallel.Strategies

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        expr <- readString
        m <- readInt
        n <- readInt
        dict <- replicateM n readString
        return (n, m, expr, dict)
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
modulo = 10009

newtype Mod = Mod Int deriving Eq

instance Show Mod where
    show (Mod a) = show a

instance Num Mod where
    Mod a + Mod b = Mod $ (a + b) `mod` modulo
    Mod a - Mod b = Mod $ (a - b) `mod` modulo
    Mod a * Mod b = Mod $ (a * b) `mod` modulo
    fromInteger a = Mod $ fromInteger a `mod` modulo
    abs = undefined
    signum = undefined

getCounts :: Char -> [ByteString] -> [Mod]
getCounts ch strs = map (Mod . BS.count ch) strs

-- choose one elements from each row of an nxm matrix
-- outcome is [[Bool]], as [1..m] is identical here
-- [[True,False,True],[False,True,False]] represents
-- a number is choosed in 1st and 3rd row, another is
-- choosed in snd row.
chooseMatrix :: Int -> Int -> [[[Bool]]]
chooseMatrix n m = map norm $ replicateM n [1..m]
  where
    norm xs = sort boolList
      where
        appeared = map head $ group $ sort xs
        boolList = map (\x -> map (==x) xs) appeared

-- the problem solved in this function
-- choose rows m times from ma, 
-- add them column by column and got the product
-- for all possible m row combination, find the sum of product
--
-- my solution is to split the product of column by column sum of m rows
-- to m^len numbers' sum, and each number here is a product of single 
-- elements from m columns, I enumerate this m^len combinations, find 
-- the identical ones ( note the order of row, in fact the times when
-- this row was selected can be ignored). I call chooseMatrix to generate
-- all possibility, and solveSet to solve single case.
--
solveTerm :: [ByteString] -> Int -> ByteString -> Mod
solveTerm dict m term = answer
  where
    n = length dict
    len = BS.length term
    termNumList = map (\ch -> getCounts ch dict) $ BS.unpack term
    ma = transpose termNumList

    matrix = chooseMatrix len m
    matrixAssocs = Map.assocs $ foldl (\map k -> Map.insertWith (+) k 1 map) Map.empty matrix

    answer = sum $ map (\(s, num) -> solveSet s * num) matrixAssocs

    solveSet :: [[Bool]] -> Mod
    solveSet s' = product [ sum [ product $ filterZip row bools | row <- ma ] | bools <- s ]
      where
        s = s' ++ replicate (m - length s') (replicate len False)
        filterZip xs bools = [x | (x,bool) <- zip xs bools, bool]

solve :: (Int, Int, ByteString, [ByteString]) -> String
solve (n, m, expr, dict) = unwords $ map show answer
  where
    terms = BS.split '+' expr
    answer = [sum $ map (solveTerm dict i) terms | i <- [1..m]]
