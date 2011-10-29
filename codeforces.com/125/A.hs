
main = interact $ solve . read

solve n = let n' = (n + 1) `div` 3 in show (n' `div` 12) ++ " " ++ show (n' `mod` 12)
