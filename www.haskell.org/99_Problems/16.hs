
dropEvery :: [a] -> Int -> [a]

dropEvery x k = drop2 x k 1 
    where 
        drop2 [] k z = []
        drop2 (x:xs) k z = 
            if k == z
            then drop2 xs k 1
            else x:(drop2 xs k (z + 1))

