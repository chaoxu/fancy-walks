
combinations2 :: Int -> [a] -> [([a],[a])]

combinations2 0 x = [([],x)]
combinations2 _ [] = [([],[])]

combinations2 n (x:xs) = 
    if n >len
    then []
    else if n == len
    then [(x:xs,[])]
    else map (\r -> (fst r, x:(snd r))) (combinations2 n xs) ++ map (\r -> (x:(fst r), snd r)) (combinations2 (n-1) xs)
    where len = (length xs) + 1

group :: [Int] -> [a] -> [[[a]]]

group [] [] = [[]]
group _ [] = []
group [] _ = []
group (x:xs) lst = 
    concatMap (\r -> map (\q -> (fst r) : q) (group xs (snd r))) (combinations2 x lst)
