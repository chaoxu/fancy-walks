
regions = [(ch, i) | ch <- "SDT", i <- [1..20]] ++ [('S', 25), ('D', 25)]

score ('S', a) = a
score ('D', a) = a * 2
score ('T', a) = a * 3

problem_109 = length checkouts
  where
    checkouts = [ lst
                | lst <- checkout1 ++ checkout2 ++ checkout3
                , let s = sum $ map score lst
                , s < 100
                ]
    checkout1 = [[x] | x <- regions, fst x == 'D']
    checkout2 = [[x, y] | x <- regions, y <- regions, fst y == 'D']
    checkout3 = [ [x, y, z]  
                | x <- regions
                , y <- regions
                , x <= y
                , z <- regions
                , fst z == 'D'
                ]

main = print problem_109
