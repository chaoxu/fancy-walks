{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE TupleSections #-}

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
import Data.Tree
import Data.Graph

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        replicateM n $ (\x -> (readRational x, x)) . BS.unpack <$> readString
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readRational str = read (filter isDigit str) % 10^(length str - dotplace - 1)
      where
        dotplace = fromMaybe (length str - 1) $ elemIndex '.' str

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, a) -> do
        putStrLn $ "Case #" ++ show cas ++ ":"
        forM_ (solve a) putStrLn

getTimes :: Rational -> State (Map Rational Int) Int
getTimes a = do
    map <- get
    if Map.member a map 
      then return (map Map.! a)
      else do
        modify $ Map.insert a maxBound
        r <- if c == 1 then return 0 else (\x -> if x == maxBound then x else x+1) <$> getTimes d
        modify $ Map.insert a r
        return r
  where
    (c, d) = properFraction (a * 3)

solve :: [(Rational,String)] -> [String]
solve a = map snd $ sort a'
  where
    monads = foldM (\xs (v,s) -> (:xs).(,s).(,v) <$> getTimes v) [] a
    a' = evalState monads Map.empty
