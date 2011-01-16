{-# OPTIONS_GHC -O2 #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List
import Data.Function
import qualified Data.Sequence as S

genFactor n = runSTUArray genFactorST
  where
    genFactorST = do
        f <- newArray (2, n) 0 :: ST s (STUArray s Int Int)
        foldM_ (helper f) S.empty [2..n]
        return f
    helper f seq i = do
        val <- readArray f i
        let seq' = if val == 0 then seq S.|> i else seq
        if val == 0 then writeArray f i i else return ()
        helper2 f seq' i
        return seq'
    helper2 f seq i = case S.viewl seq of
        S.EmptyL -> return ()
        h S.:< seq' -> if h * i > n then return () else do
            writeArray f (h * i) h
            if i `mod` h == 0 then return () else
                helper2 f seq' i
            
getFactors f n = unfoldr (\x -> if x == 1 then Nothing else let d = f ! x in Just (d, div x d)) n 

problem_69 = fst $ maximumBy (compare `on` snd) [(n, fromIntegral n / fromIntegral (phi n)) | n <- [2..limit]]
  where
    limit = 1000000
    fs = genFactor limit
    phi n = n `div` product pfs * product pfs'
      where
        pfs = nub $ getFactors fs n
        pfs' = map pred pfs

main = print problem_69
