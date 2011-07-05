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
        int <- BS.unpack <$> readString
        source <- BS.unpack <$> readString
        target <- BS.unpack <$> readString
        return (int, source, target)
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

solve :: (String, String, String) -> String
solve (int, source, target) = map (tmap!) . toI m . fromI n . map (smap!) $ int
  where
    n = length source
    m = length target

    smap = array (chr 0, chr 255) $ zip source [0..] :: UArray Char Int
    tmap = listArray (0, m-1) target :: UArray Int Char

fromI :: Int -> [Int] -> Integer
fromI base num = go num 0
  where
    go [] val = val
    go (x:xs) val = go xs (val * fromIntegral base + fromIntegral x)

toI :: Int -> Integer -> [Int]
toI base val = reverse $ map fromIntegral num
  where
    num = unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` fromIntegral base, x `div` fromIntegral base)) val
