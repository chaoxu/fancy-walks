
import Data.List

solve :: [Integer] -> Integer
solve [x,y,z,k] = maximum ans
  where
    arr = [x-1,y-1,z-1]
    bsearch lo hi
        | lo >= hi = lo
        | sum (map (min (mid+1)) arr) <= k = bsearch (mid + 1) hi
        | otherwise = bsearch lo mid
      where
        mid = (lo + hi) `div` 2
    bal = bsearch 0 k
    arr2 = map (min bal) arr :: [Integer]
    count = k - sum arr2
    ans = [ (x' + dx + 1) * (y' + dy + 1) * (z' + dz + 1)
          | let [x',y',z'] = arr2
          , dx <- check x' x
          , dy <- check y' y
          , dz <- check z' z
          , dx + dy + dz <= count
          ]
    check a' a = if a' + 1 < a then [0,1] else [0]


main = interact $ show . solve . map read . take 4 . words

