
slice :: [a] -> Int -> Int -> [a]

slice [] _ _ = []
slice (x:xs) l r = 
    if (l <= 1 && 1 <= r)
    then x:right
    else right
    where right = slice xs (l-1) (r-1)
