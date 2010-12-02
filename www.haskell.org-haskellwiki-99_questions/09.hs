
pack :: Eq a => [a] -> [[a]]

pack = foldr helper [[]]
    where 
        helper l [[]] = [[l]]
        helper l r = if (l == (head $ head r)) then (l:head r):(tail r) else ([l]:r)
