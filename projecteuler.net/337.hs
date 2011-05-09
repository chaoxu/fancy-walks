{-# OPTIONS_GHC -O2 #-} -- run with +RTS -K512m -A32m
{-# LANGUAGE RankNTypes #-}

--
--
-- warning!! dirty and imperative solution starts
--
--

import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.STRef
import Data.Bits
import Control.Applicative

modulo = 10^8

add a b = if s >= modulo then s - modulo else s
 where
    s = a + b
minus a b = if s < 0 then s + modulo else s
  where
    s = a - b

addDelta bit x limit delta = do
    forM_ lst $ \i -> do
        biti <- readArray bit i
        writeArray bit i (add biti delta)
  where
    lst = takeWhile (<limit) $ iterate (\x -> x + (x .&. negate x)) x

getSum bit x = do
    res <- newSTRef 0 :: forall s . ST s (STRef s Int)
    forM_ lst $ \i -> do
        biti <- readArray bit i
        modifySTRef res (add biti)
    readSTRef res
  where
    lst = takeWhile (>0) $ iterate (\x -> x - (x .&. negate x)) x

solve n = do
    bit <- newArray (1,n) 0 :: forall s . ST s (STUArray s Int Int)
    liftM last $ forM [n,n-1..6] $ \k -> do
        sum' <- liftM2 minus (getSum bit (k - 1)) (getSum bit (phi k))
        let sum = add sum' 1
        addDelta bit (phi k) k sum
        return sum
  where
    phi = (cache!)
      where
        cache = runSTUArray $ do
            phi <- newListArray (1, n) [0..n-1] :: forall s . ST s (STUArray s Int Int)
            forM_ [2..n] $ \i -> do
                phii <- readArray phi i
                when (phii == i - 1) $ forM_ [i*2,i*3..n] $ \j -> do
                    phij <- readArray phi j
                    writeArray phi j (phij - phij `div` i)
            return phi
    
problem_337 n = runST $ solve n

main = print $ problem_337 20000000
