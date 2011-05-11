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

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        BS.unpack <$> readString
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ (solve params)

solve line
    | hi' == [] = (head sortwo0):(replicate (count0+1) '0' ++ tail sortwo0)
    | otherwise = reverse $ (reverse . sort) (lo'' ++ [choose] ++ tail hi'') ++ [head hi''] ++ tail hi
  where
    count0 = length $ filter (=='0') line
    sortwo0 = sort $ filter (/='0') line
    
    line' = reverse line
    (lo', hi') = span (uncurry (<=)) $ zip line' (tail line'++[maxBound])
    lo = map fst $ lo' ++ [head hi']
    hi = map fst $ tail hi'
    choose = head hi
    (lo'', hi'') = span (<=choose) lo
