
repli :: [a] -> Int -> [a]

repli [] _ = []
repli _ 0 = []
repli (x:xs) n = foldr (\a b -> x:b) (repli xs n) [1..n]
