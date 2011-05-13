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
import Data.Tree
import Data.Graph

gcd' 0 0 = 0
gcd' a b = gcd a b

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        replicateM n readInteger
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, a) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve a)

solve :: [Integer] -> Integer
solve a =
    if res == 0 then 0 else g - res
  where
    diff = map (\x -> x-(head a)) $ tail a
    g = foldl gcd' 0 diff
    res = head a `mod` g
