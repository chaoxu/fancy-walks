
range :: Int -> Int -> [Int]

range l r = 
    if r < l
    then [] 
    else l:(range (l+1) r)
