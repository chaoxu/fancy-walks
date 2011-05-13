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
import Control.Parallel

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        replicateM 4 readInt
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve params)

solve [a1, a2, b1, b2] = solve' [a1, a2, b1, b2] + solve' [b1, b2, a1, a2]
solve' [a1, a2, b1, b2] = goall (a1, a2)
  where
    goall (l, r) 
        | r - l < 100 = go (l, r)
        | otherwise   = lo `par` (hi `pseq` (lo + hi))
      where
        mid = (l + r) `div` 2
        lo = goall (l, mid)
        hi = goall (mid + 1, r)

    go (l, r) = sum [ fromIntegral (b2 - b1' + 1) `max` 0 :: Int64
                    | a <- [l..r]
                    , let b1' = b1 `max` getBound a
                    ]
    golden = (1 + sqrt 5) / 2 :: Double
    getBound a = ceiling (golden * fromIntegral a) :: Int
