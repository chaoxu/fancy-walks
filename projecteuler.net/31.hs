
search num _ | num < 0 = 0
search 0 _ = 1
search _ [] = 0
search num a@(x:xs) = search num xs + search (num - x) a

problem_31 = search 200 [200,100,50,20,10,5,2,1]

main = print problem_31
