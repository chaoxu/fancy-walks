
elementAt :: [a] -> Int -> a

elementAt a 1 = head a
elementAt a b = elementAt (tail a) (b - 1)
