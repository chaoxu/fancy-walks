{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe
import Data.Char
import Data.Array
import Data.Int
import Control.Monad.State
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS

swap (a,b) = (b,a)

parseInput = do 
    n <- readInt
    k <- replicateM n readInt
    edges <- replicateM (n-1) ((,) <$> readInt <*> readInt)
    s <- readInt
    return (n,k,edges,s)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace

solve n s karr adj = fst $ dfs s 0
  where
    dfs u p = (res, ku')
      where
        ku = karr ! u - if p == 0 then 0 else 1
        chd = reverse $ sort [dfs v u | v <- adj ! u, v /= p]
        visitedChd = take ku chd
        allLeftK = sum $ map (fromIntegral.snd) visitedChd :: Int64
        leftK = fromIntegral allLeftK `min` (ku - length visitedChd)
        ku' = ku - (length visitedChd) - leftK
        res = sum (map ((+2).fst) visitedChd) + fromIntegral leftK * 2 :: Int64

main = do
    (n,k,edges,s) <- evalState parseInput <$> BS.getContents
    let karr = listArray (1,n) k
    let adj = accumArray (flip (:)) [] (1,n) $ edges ++ map swap edges
    print $ solve n s karr adj
