
compress :: Eq a => [a] -> [a]

compress = foldr helper []
    where helper l r = if ((r /= []) && (l == head r)) then r else (l:r)

