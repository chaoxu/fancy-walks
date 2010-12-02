
insertAt :: a -> [a] -> Int -> [a]

insertAt ch [] 1 = [ch]
insertAt _ [] _ = []
insertAt ch (x:xs) pos = 
    if pos == 1
    then ch:remaining
    else remaining
    where remaining = x:(insertAt ch xs (pos - 1))
