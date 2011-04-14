{-# OPTIONS_GHC -O2 #-}

import Data.Int
import Data.Monoid
import Data.Maybe
import Data.Array
import Control.Applicative
import qualified Data.ByteString.Char8 as BS

--import Control.Parallel
import qualified GHC.Conc (par, pseq)
infixr 0 `par`, `pseq`
par = GHC.Conc.par
pseq = GHC.Conc.pseq

foldMap :: Monoid m => (a -> m) -> [a] -> m
foldMap f x = foldMap' x (length x)
  where
    foldMap' xs ln
        | ln == 0    = mempty
        | ln >= 100  = zs' `par` (ys' `pseq` (ys' `mappend` zs'))
        | otherwise  = foldl1 mappend $ map f xs
      where
        lny     = ln `div` 2
        lnz     = ln - lny
        (ys,zs) = splitAt lny xs
        ys'     = foldMap' ys lny
        zs'     = foldMap' zs lnz

data MaxSum a = MaxSum
              { prefixS :: a
              , middleS :: a
              , suffixS :: a
              , allS :: a
              } deriving Show

instance (Bounded a, Num a, Ord a) => Monoid (MaxSum a) where
    mempty = MaxSum minBound minBound minBound 0
    mappend x y = MaxSum pre mid suf sum
      where
        pre = prefixS x `max` (allS x + prefixS y)
        mid = middleS x `max` middleS y `max` (suffixS x + prefixS y)
        suf = suffixS y `max` (allS y + suffixS x)
        sum = allS x + allS y

singleton :: Int -> MaxSum Int64
singleton x = MaxSum x' x' x' x'
  where
    x' = fromIntegral x

readI = fst . fromJust . BS.readInt

main = do
    [n, m] <- map read . words <$> getLine :: IO [Int]
    lines <- BS.lines <$> BS.getContents
    let states = map (foldMap singleton . tail . map readI . BS.words) $ take n lines
    let list = map readI . take m . BS.words . head $ drop n lines
    let arr = listArray (1, n) states :: Array Int (MaxSum Int64)
    let answer = foldMap (arr!) list
    print $ middleS answer
