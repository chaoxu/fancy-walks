
import Data.List
import Data.Array.Unboxed
import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as BS
 
pfactors n toTest@(x:xs) 
    | n <= 1         = []
    | x * x > n      = [n]
    | n `mod` x == 0 = x:pfactors (n `div` x) toTest
    | otherwise      = pfactors n xs

divisorsFromF xs = sort . map product . sequence . map (scanl (*) 1) . group $ sort xs

readI = fst . fromJust . BS.readInt

--return the highest value in a sorted array LEQ x
solve :: UArray Int Int -> Int -> Maybe Int
solve sorted x = if sorted ! pos <= x then Just (sorted ! pos) else Nothing
  where
    bnds = bounds sorted
    bsearch lo hi
        | lo >= hi              = lo
        | sorted ! (mid+1) <= x = bsearch (mid + 1) hi
        | otherwise             = bsearch lo mid
      where
        mid = (lo + hi) `div` 2
    pos = bsearch (fst bnds) (snd bnds)

main = do
    [x, y] <- map read . words <$> getLine :: IO [Int]
    let g = gcd x y
    let factors = pfactors g [2..]
    let divisors = divisorsFromF factors
    let arr = listArray (1, length divisors) divisors :: UArray Int Int
    q <- read <$> getLine :: IO Int
    lines <- map (map readI . BS.words) . take q . BS.lines <$> BS.getContents
    forM_ lines $ \[low, high] -> do
        let val = solve arr high
        let answer = if isJust val && fromJust val >= low then fromJust val else -1
        print answer
