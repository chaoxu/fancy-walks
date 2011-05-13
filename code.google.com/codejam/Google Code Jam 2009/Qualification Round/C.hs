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
        BS.unpack <$> readLine
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve params)

modulo = 10000
newtype Mod = Mod Int deriving Eq

instance Show Mod where
    show (Mod a) = let str = show a in replicate (4 - length str) '0' ++ str

instance Num Mod where
    Mod a + Mod b = Mod $ (a + b) `mod` modulo
    Mod a - Mod b = Mod $ (a + modulo - b) `mod` modulo
    Mod a * Mod b = Mod $ (a * b) `mod` modulo
    fromInteger a = Mod $ fromInteger a
    signum = undefined
    abs = undefined

solve line = ways (0,0)
  where
    pattern = "welcome to code jam"
    n = length line
    m = length pattern

    lineA = listArray (0,n-1) line
    patternA = listArray (0,m-1) pattern

    ways :: (Int, Int) -> Mod
    ways = (cache!)
      where
        bnds = ((0,0),(n,m))
        cache = listArray bnds [go idx | idx <- range bnds]

        go (x, y) 
            | x == n    = if y == m then 1 else 0
            | otherwise = choose + notChoose
          where
            choose = if y < m && lineA ! x == patternA ! y then ways (x + 1, y + 1) else 0
            notChoose = ways (x + 1, y)
