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
import Debug.Trace

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        grid <- replicateM n (BS.unpack <$> readString)
        return (n, m, grid)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStr $ "Case #" ++ show cas ++ ": " ++ (solve params)

readHex :: Char -> [Bool]
readHex = reverse . map odd . take 4 . iterate (`div` 2) . digitToInt

type Node = Array (Int,Int) (Maybe Bool)

solve (n, m, grid') = unlines $ show (length res) : map (\(x,y) -> (show x ++ " " ++ show y)) res
  where
    bnds = ((0,0),(n-1,m-1))
    grid'' = map (concatMap readHex) grid' :: [[Bool]]
    grid = [[even (x + y) == e | (y, e) <- zip [0..] row] | (x, row) <- zip [0..] grid''] :: [[Bool]]

    initA = listArray bnds [Just e | row <- grid, e <- row] :: Node

    res = evalState go initA :: [(Int, Int)]

    go :: State Node [(Int,Int)]
    go = do
        arr <- get
        let (k, op) = getOperation arr
        if k == -1 then
            return []
          else do
            let difflist = concatMap (\(x,y) -> [((x+dx, y+dy),Nothing) | dx <- [0..k-1], dy <- [0..k-1]]) op
            let arr' = arr // difflist
            put arr'
            ((k, length op):) <$> go

getOperation arr = (k, Set.toList res)
  where
    bnds = bounds arr
    (n, m) = (nn + 1, nm + 1)
      where
        (_, (nn, nm)) = bnds
    
    buildCountArray :: Array (Int,Int) Bool -> Array (Int,Int) Int
    buildCountArray a = listArray ((0,0),(n,m)) [elem | row <- sumAll, elem <- take (m+1) row]
      where
        a' = [[if val then 1 else 0 | y <- [0..m-1], let val = a ! (x,y)] | x <- [0..n-1]]
        sumRow = map (scanl (+) 0) a'
        sumAll = scanl (zipWith (+)) (repeat 0) sumRow
        
    build bool = buildCountArray $ fmap (==(Just bool)) arr :: Array (Int,Int) Int

    arrT = build True
    arrF = build False

    all = listArray bnds [ if isNothing val then 0 else bsearch idx (fromJust val)
                         | idx <- range bnds
                         , let val = arr ! idx
                         ]
    
    bsearch (x, y) bool = go 1 ((n - x) `min` (m - y))
      where
        arr = if bool then arrT else arrF
        calc (nx, ny) = arr ! (x, y) + arr ! (nx, ny) - arr ! (nx, y) - arr ! (x, ny)
        go lo hi
            | lo == hi       = lo
            | real == expect = go (mid + 1) hi
            | otherwise      = go lo mid
          where
            mid = (lo + hi) `div` 2
            real = calc (x + mid + 1, y + mid + 1)
            expect = (mid + 1) ^ 2

    k = let k' = maximum $ map snd $ assocs all in if k' == 0 then -1 else k'

    set = [idx | idx <- range bnds, all ! idx == k]

    res = foldl (\s (x,y) -> if check s (x,y) then Set.insert (x,y) s else s) Set.empty set

    check s (x,y) = not $ any (flip Set.member s) [(x-dx, y-dy) | dx <- [0..k-1], dy <- [(1-k)..k-1]]

