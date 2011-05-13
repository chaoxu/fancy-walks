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
        n <- readInt
        town <- readInt
        e <- readInt
        a <- replicateM e $ (,) <$> readInt <*> readInt
        return (town, n, a)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ (solve params)

inf = 10^9

solve (town, n, a)
    | any isNothing ans = "IMPOSSIBLE"
    | otherwise         = unwords $ map (show . fromJust) ans
  where
    ans = map go [1..n]
    go :: Int -> Maybe Int
    go ptown
        | ptown == town = Just 0
        | n > sum lst   = Nothing
        | otherwise     = Just $ fst . head $ dropWhile ((<n).snd) $ zip [0..] psum
      where
        n = length lst
        lst = reverse $ sort [ canTake | (home, canTake) <- a, home == ptown]
        psum = scanl (+) 0 lst
