{-# OPTIONS_GHC -O2 #-}

import Data.Int
import Data.Monoid
import Data.Maybe
import Data.Array
import Control.Applicative
import qualified Data.ByteString.Char8 as BS

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
    mconcat [] = mempty
    mconcat xs = foldl1 mappend xs

singleton :: Int -> MaxSum Int64
singleton x = MaxSum x' x' x' x'
  where
    x' = fromIntegral x

readI = fst . fromJust . BS.readInt

main = do
    [n, m] <- map read . words <$> getLine :: IO [Int]
    lines <- BS.lines <$> BS.getContents
    let states = map (mconcat . map singleton . tail . map readI . BS.words) $ take n lines
    let list = map readI . take m . BS.words . head $ drop n lines
    let arr = listArray (1, n) states :: Array Int (MaxSum Int64)
    let answer = mconcat $ map (arr!) list
    print $ middleS answer
