
combinations :: Int -> [a] -> [[a]]

combinations 0 _ = [[]]
combinations _ [] = [[]]

combinations n (x:xs) = 
    if n >len
    then []
    else if n == len
    then [x:xs]
    else (combinations n xs) ++ map (\r -> x:r) (combinations (n-1) xs)
    where len = (length xs) + 1
