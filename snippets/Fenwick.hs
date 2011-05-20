
import Data.Bits
import Data.Array.MArray
import Control.Monad

lowBit :: (Bits i, Num i) => i -> i
lowBit x = x .&. negate x

getSum :: (Ix i, Bits i, Num i, Num e, MArray a e m) => a i e -> (i, i) -> i -> m e
getSum fenwick bnds x = sum `liftM` mapM (readArray fenwick) xs
  where
    xs = takeWhile (inRange bnds) $ iterate (\i -> i - lowBit i) x

addDelta :: (Ix i, Bits i, Num i, Num e, MArray a e m) => a i e -> (i, i) -> i -> e -> m ()
addDelta fenwick bnds x delta =
    forM_ xs $ \i -> do
        fi <- readArray fenwick i
        writeArray fenwick i (fi + delta)
  where
    xs = takeWhile (inRange bnds) $ iterate (\i -> i + lowBit i) x

