import Data.Function
import List

main = readLn >>= putStrLn . solve

solve n = show k ++ "\n" ++ unlines [unwords $ map (show . snd) day | day <- lst3 ]
  where
    k = last $ takeWhile (\x -> x * (x - 1) `div` 2 <= n) [1..]
    lst = [(i, j) | i <- [1..k], j <- [1..i-1]]

    lst2 = concat [[(pi, i), (pj, i)] | (i, (pi, pj)) <- zip [1..] lst]

    lst3 = groupBy ((==) `on` fst) $ sort lst2
