myButLast :: [a] -> a

myButLast (_:x:y:xs) = myButLast (x:y:xs)
myButLast (x:y:xs) = x
