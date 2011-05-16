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

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        replicateM 8 readInteger
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

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

solve :: [Integer] -> Integer
solve [n, a, b, c, d, x0, y0, modulo] = sum lst
  where
    genList (a, b, x0, modulo) = iterate (\x -> (a * x + b) `mod` modulo) x0
    xs = map (`mod` 3) $ genericTake n $ genList (a, b, x0, modulo)
    ys = map (`mod` 3) $ genericTake n $ genList (c, d, y0, modulo)
    pts = zip xs ys
    
    bnds = ((0,0),(2,2))
    counts = accumArray (+) 0 bnds [ (pt, 1) | pt <- pts]

    lst = [ product $ zipWith combine (elems counts) (elems counts')
          | a <- range bnds
          , b <- range bnds
          , a <= b
          , c <- range bnds
          , b <= c
          , (fst a + fst b + fst c) `mod` 3 == 0
          , (snd a + snd b + snd c) `mod` 3 == 0
          , let counts' = accumArray (+) 0 bnds [(a, 1), (b, 1), (c, 1)]
          ]

    combine :: Integer -> Integer -> Integer
    combine x y
        | y < 0 || y > x   = 0
        | y == 0 || y == x = 1
        | otherwise        = product [x-y+1..x] `div` product [1..y]
