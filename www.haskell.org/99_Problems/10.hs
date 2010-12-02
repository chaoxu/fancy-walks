
encode :: Eq a => [a] -> [(Int, a)]

encode = foldr helper [] 
    where 
        helper l [] = [(1,l)]
        helper l r = if (l == (snd $ head r)) then (1 + (fst $ head r),snd $ head r):(tail r) else ((1, l):r)

