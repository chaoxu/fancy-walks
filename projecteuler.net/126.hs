
unionInf :: (Ord a, Num b) => [(a, b)] -> [(a, b)] -> [(a, b)]
unionInf (x:xs) (y:ys) = case compare (fst x) (fst y) of
    LT -> x : unionInf xs (y:ys)
    GT -> y : unionInf (x:xs) ys
    EQ -> (fst x, snd x + snd y) : unionInf xs ys

joinInf :: (Ord a, Num b) => [[(a, b)]] -> [(a, b)]
joinInf ((x:xs):ys) = x : unionInf xs (joinInf (pairsInf ys))

pairsInf :: (Ord a, Num b) => [[(a, b)]] -> [[(a, b)]]
pairsInf ((x:xs):ys:remain) = (x : unionInf xs ys) : pairsInf remain

solve x y z k = faces + edges + vertices
  where
    faces = (x * y + y * z + z * x) * 2
    edges = (x * 4 + y * 4 + z * 4) * (k - 1)
    vertices = (k - 1) * (k - 2) `div` 2 * 8

counts = joinInf [joinInf [joinInf [[(solve x y z k, 1) | k <- [1..]] | z <- [y..]] | y <- [x..]] | x <- [1..]]

problem_126 = head [x | (x, ways) <- counts, ways == 1000]

main = print problem_126
