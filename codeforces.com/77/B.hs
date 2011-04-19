{-# OPTIONS_GHC -O2 #-}

import Data.Int
import Data.Maybe
import Data.Ratio
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as BS

-- p >= 4q, p \in [0,a], q \in [-b,b]
solve :: Integer -> Integer -> Double
solve a 0 = 1.0
solve 0 b = 0.5
solve a b = fromRational (area8 % (area * 8)) * 0.5 + 0.5
  where
    area8
        | a >= 4 * b = (a - 4 * b) * b * 4 + a * b * 4
        | otherwise  = a * a
    area = a * b

readI = fst . fromJust . BS.readInteger

main = do
    t <- read <$> getLine :: IO Int
    query <- map (map readI . BS.words) . BS.lines <$> BS.getContents
    forM_ query $ \[a,b] -> do
        print $ solve a b

