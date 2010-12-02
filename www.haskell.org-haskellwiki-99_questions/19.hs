
rotate :: [a] -> Int -> [a]

rotate a 0 = a
rotate a n = 
    if n > 0
    then rotate ((tail a) ++ [head a]) (n - 1)
    else rotate a ((length a) + n)
