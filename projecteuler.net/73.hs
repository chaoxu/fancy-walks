{-# OPTIONS_GHC -O2 #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List
import Data.Function
import qualified Data.Sequence as S

gen n func = runSTUArray genST
  where
    genST = do
        f <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
        forM_ [1..n] $ \i -> do
            writeArray f i (func i)
        forM_ [1..n] $ \i -> do
            vi <- readArray f i
            forM_ [i*2,i*3..n] $ \j -> do
                vj <- readArray f j
                writeArray f j (vj - vi)
        return f

solve n (p,q) = sum $ map (toInteger.snd) $ assocs g
  where
    g = gen n (\x -> div (x * p) q)

problem_73 = solve limit (1,2) - solve limit (1,3) - 1
  where
    limit = 12000

main = print problem_73
