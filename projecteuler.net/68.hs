
import Data.List
import Data.Function
import Data.Maybe

rotate arr = tail arr ++ [head arr]
rotateTo el arr = t ++ h
  where
    pos = fromJust $ elemIndex el arr
    (h,t) = splitAt pos arr

n = 5

list = [ (innerI, outerI)
       | let range = [1..n*2]
       , inner <- filter ((==n).length) $ subsequences range
       , let s5 = sum range + sum inner
       , s5 `mod` n == 0
       , let s = s5 `div` n
       , innerI <- permutations inner
       , let outerI = zipWith (\x y -> s - x - y) innerI (rotate innerI)
       , sort (innerI ++ outerI) == range
       ]

genDigit (inner, outer) = 
    concat $ rotateTo max_tuple tuples
  where
    inner' = rotate inner
    tuples = zipWith3 (\x y z -> [x,y,z]) outer inner inner'
    max_tuple = minimumBy (compare `on` head) tuples

problem_68 = maximum $ nub . sort $ filter ((==16).length) $ map (concatMap show . genDigit) $ nub . sort $ list

main = putStrLn problem_68
