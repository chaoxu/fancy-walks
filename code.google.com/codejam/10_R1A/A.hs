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
        putStrLn $ "Case #" ++ show cas ++ ": " ++ (solve params)

solve (n, m, grid)
    | isR && isB = "Both"
    | isR        = "Red"
    | isB        = "Blue"
    | otherwise  = "Neither"
  where
    grid2 = map (filter (/='.')) grid
    grid' = map (\x -> replicate (n - length x) '.' ++ x) grid2
    
    isR = check m $ map (map (=='R')) grid'
    isB = check m $ map (map (=='B')) grid'

check :: Int -> [[Bool]] -> Bool
check m grid = any go [(1,1), (1,0), (0,1), (1,-1)]
  where
    n = length grid
    bnds = ((0,0),(n-1,n-1))
    a = listArray bnds [elem | row <- grid, elem <- row]
    go (dx,dy) = (>=m) . maximum . map snd $ assocs arr
      where
        arr = listArray bnds [ if okay then arr ! (nx,ny) + 1 else if a ! (x,y) then 1 else 0
                             | (x,y) <- range bnds
                             , let (nx,ny) = (x+dx,y+dy)
                             , let okay = inRange bnds (nx,ny) && a ! (x,y)
                             ]
