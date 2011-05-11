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
import Data.Graph

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        n <- readInt
        m <- readInt
        a <- replicateM m readInt
        return (n, a)
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

solve (n, a) = release (0, m-1)
  where
    set' = Set.toList . Set.fromList $ a
    set = [0] ++ set' ++ [n + 1]
    seg' = sort $ filter (uncurry (<=)) (zipWith (\x y -> (x+1,y-1)) set (tail set)) ++ map (\x -> (x,x)) set'
    seg = listArray (0, m - 1) seg'
    segR = IntMap.fromList [(k,v) | k <- set', let Just v = elemIndex (k,k) seg']
    m = length seg'

    release idx = if inRange bnds idx then (cache!idx) else 0
      where
        bnds = ((0,0), (m-1, m-1))
        cache = listArray bnds $ map go $ range bnds

        go (x, y)
            | x > y     = 0
            | lst == [] = 0
            | otherwise = minimum lst + (ry - lx)
          where
            lst = [release (x, sp'-1) + release (sp'+1, y)| sp <- set', lx <= sp && sp <= ry, let sp' = segR IntMap.! sp]
            (lx, rx) = seg ! x
            (ly, ry) = seg ! y
