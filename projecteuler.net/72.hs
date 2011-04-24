{-# OPTIONS_GHC -O2 #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List
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
        when (val == 0) $ writeArray f i i
        helper2 f seq' i
        return seq'
    helper2 f seq i = case S.viewl seq of
        S.EmptyL -> return ()
        h S.:< seq' -> unless (h * i > n) $ do
            writeArray f (h * i) h
            unless (i `mod` h == 0) $ helper2 f seq' i
            
getFactors f = unfoldr (\x -> if x == 1 then Nothing else let d = f ! x in Just (d, div x d))

problem_72 = sum [toInteger (phi n) | n <- [2..limit]]
  where
    limit = 1000000
    fs = genFactor limit
    phi n = n `div` product pfs * product pfs'
      where
        pfs = nub $ getFactors fs n
        pfs' = map pred pfs

main = print problem_72
