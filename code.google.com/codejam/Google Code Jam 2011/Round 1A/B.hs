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
        dict <- replicateM n $ (BS.unpack <$> readString)
        orders <- replicateM m $ (BS.unpack <$> readString)
        return (n, dict, orders)
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

spChar :: Char
spChar = chr (ord 'a' - 1)

appearMask :: String -> [Int]
appearMask str = elems arr
  where
    bnds = (spChar,'z')
    arr = accumArray (.|.) 0 bnds ((spChar, length str) : [ (ch, 1 `shiftL` id) | (id, ch) <- zip [0..] str]) :: UArray Char Int

solve :: (Int, [String], [String]) -> String
solve (n, dict, orders) = unwords $ map ((dict'!) . solveCase n masks) orders
  where
    dict' = listArray (1, n) dict :: Array Int String
    bnds = ((1, spChar), (n, 'z'))
    masks = listArray bnds [ msk | word <- dict, msk <- appearMask word] :: UArray (Int, Char) Int

solveCase :: Int -> UArray (Int, Char) Int -> String -> Int
solveCase n masks order = ansID
  where
    lst = go 0 (spChar:order) [1..n]
    go _ _ [] = []
    go bonus _ [x] = [(bonus, -x)]
    go bonus (x:xs) lst
        | length sp == 1 = go bonus xs lst
        | otherwise      = concatMap (go (bonus + 1) xs) lo ++ concatMap (go bonus xs) hi
      where
        cmpFunc = compare `on` (\p -> masks ! (p, x))
        eqlFunc = (==) `on` (\p -> masks ! (p, x))
        sp = groupBy eqlFunc . sortBy cmpFunc $ lst
        (lo, hi) = span (\(p:_) -> masks ! (p, x) == 0) sp
    ansID = negate . snd $ maximum lst
