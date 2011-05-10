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
import qualified Data.Foldable as F
import Data.Tree
import Data.Graph
import Debug.Trace

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        a <- replicateM m $ do
            i <- readInt
            op <- BS.unpack <$> readString
            if op == "T" || op == "L" then
                (op == "L",).(i:) <$> replicateM 1 readInt
              else
                (op == "D",).(i:) <$> replicateM 2 readInt
        return (n, a)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ (solve params)

solve (n, a) = case gauss ma of
      Nothing -> "IMPOSSIBLE"
      Just res  -> unwords $ map resolv res
  where
    bnds = (1,n)
    ma = map (\(res,row) -> (buildRow row ++ [res])) a
    buildRow = map snd . assocs . accumArray (/=) False bnds . map (\x -> (x, True))

    resolv (Just (True, [])) = "L"
    resolv (Just (False, [])) = "T"
    resolv _ = "-"

gauss :: [[Bool]] -> Maybe [Maybe (Bool,[Int])]
gauss ma = fmap (map snd . assocs) . go $ map (none,) ma
  where
    vars = length (head ma) - 1
    bnds = (1, vars)
    none = -1

    go :: [(Int,[Bool])] -> Maybe (Array Int (Maybe (Bool,[Int])))
    go ma@((_,[_]):_)
        | elem (none, [True]) ma = Nothing
        | otherwise              = Just $ accumArray (flip const) Nothing bnds [(idx, Just (rhs, [])) | (idx,[rhs]) <- ma, idx /= none]

    go ma
        | null hi   = fmap addFreeDep $ go (reduce ma)
        | otherwise = go (reduce ma')
      where
        cur = vars - length (snd $ head ma) + 2
        (lo, hi) = span (\(x,row) -> x /= none || not (head row)) ma
        reduce = map (fmap tail)

        pivot = snd $ head hi
        rowXor a b = if head a == head b then zipWith (/=) a b else b

        ma' = map (fmap $ rowXor pivot) lo ++ [(cur, pivot)] ++ map (fmap $ rowXor pivot) (tail hi)

        trans = flip (fmap . fmap . (:))
        addFreeDep = flip (accum trans) [(val, cur) | (val,True:_) <- ma, val /= none]
