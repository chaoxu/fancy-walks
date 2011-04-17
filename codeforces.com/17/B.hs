
import Control.Applicative
import Control.Monad
import Data.Function
import Data.Array

solve :: Array Int Int -> Int
solve arr = if z > 1 then -1 else s
  where
    func (a, b) c
        | c == maxBound = (a, b+1)
        | otherwise     = (a+c, b)
    (s, z) = foldl func (0, 0) $ map snd $ assocs arr

main = do
    n <- read <$> getLine :: IO Int
    q <- map read . words <$> getLine :: IO [Int]
    let parseLine [a,b,c] = (read b, read c)
    lists <- read <$> getLine >>= flip replicateM (parseLine . words<$> getLine)
    let arr = accumArray min maxBound (1,n) lists
    print $ solve arr
