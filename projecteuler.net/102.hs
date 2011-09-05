

det (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

area (x1, y1) (x2, y2) (x3, y3) = abs $ det (x2 - x1, y2 - y1) (x3 - x1, y3 - y1)

check [x1, y1, x2, y2, x3, y3]
    | abo == 0 || aco == 0 || bco == 0 = False
    | abc == abo + aco + bco           = True
    | otherwise                        = False
  where
    a = (x1, y1)
    b = (x2, y2)
    c = (x3, y3)

    o = (0, 0)

    abc = area a b c
    abo = area a b o
    aco = area a c o
    bco = area b c o

problem_102 text = length [triangle | triangle <- triangles, check triangle]
  where
    triangles = map (\line -> read $ "[" ++ line ++ "]") $ lines text :: [[Int]]

main = readFile "input/triangles.txt" >>= print . problem_102
