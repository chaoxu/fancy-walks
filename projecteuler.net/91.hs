
limit = 50

points = [(x, y) | x <- [0..limit], y <- [0..limit]]

origin = (0, 0)

distanceSq (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

problem_91 = length [ (p, q)
                    | p <- points
                    , q <- points
                    , p /= origin && q /= origin && p < q
                    , let op = distanceSq origin p
                    , let oq = distanceSq origin q
                    , let pq = distanceSq p q
                    , op + oq + pq == (op `max` oq `max` pq) * 2
                    ]

main = print problem_91
