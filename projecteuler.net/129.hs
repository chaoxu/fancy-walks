
solve n = go 1 1
  where
    go pos v | v == 0    = pos
             | otherwise = go (pos + 1) ((v * 10 + 1) `mod` n)

limit = 10^6

problem_129 = head [n | n <- [limit..], n `mod` 5 /= 0 && odd n, solve n > limit]

main = print problem_129
