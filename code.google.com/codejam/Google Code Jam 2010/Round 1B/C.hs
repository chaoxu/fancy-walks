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

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        readInt
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve params)

modulo = 100003
newtype Mod = Mod Int64 deriving (Eq, Ord)

instance Show Mod where
    show (Mod a) = show a

instance Num Mod where
    Mod a + Mod b = Mod $ (a + b) `mod` modulo
    Mod a - Mod b = Mod $ (a + modulo - b) `mod` modulo
    Mod a * Mod b = Mod $ (a * b) `mod` modulo
    fromInteger a = Mod $ fromInteger (a `mod` toInteger modulo)
    abs = undefined
    signum = undefined

combine :: (Int, Int) -> Mod
combine = (cache !)
  where
    bnds = ((0,0),(512,512))
    cache = listArray bnds $ map go $ range bnds
    go (x, y) | y < 0 || y > x = 0
    go (x, y) | y == 0 || y == x = 1
    go (x, y) = combine (x-1, y) + combine (x-1,y-1)

pureNum :: (Int, Int) -> Mod
pureNum = (cache !)
  where
    bnds = ((1,1),(512,512))
    cache = listArray bnds $ map go $ range bnds
    go (n, 1) = 1
    go (n, k) | k >= n = 0
    go (n, k) = sum [pureNum (k, pk) * combine (n - k - 1, k - pk - 1) | pk <- [1..k-1]]

solve n = sum [pureNum (n, k) | k <- [1..n]]

