
myReverse :: [a] -> [a]

myReverse (x:xs) = ((myReverse xs) ++ [x])
myReverse [] = []
