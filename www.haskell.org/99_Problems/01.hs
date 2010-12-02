myLast :: [a] -> a

myLast (_:x:xs) = myLast (x:xs)
myLast (x:xs) = x
