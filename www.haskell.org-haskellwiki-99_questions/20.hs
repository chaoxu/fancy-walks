
removeAt :: Int -> [a] -> (a, [a])

removeAt pos a = (a !! pos, removeAt2 pos a)
    where 
        removeAt2 pos [] = []
        removeAt2 pos (x:xs) = 
            if (pos == 0)
            then remaining
            else x:remaining
            where remaining = removeAt2 (pos-1) xs
