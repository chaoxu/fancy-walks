

nonDecreasing :: Ord a => [a] -> Bool
nonDecreasing a = and $ zipWith (<=) a (tail a)

checkBouncy n = not (nonDecreasing s || nonDecreasing (reverse s))
  where
    s = show n

solve percent = go 0 0
  where
    go n bn
        | bn' * 100 == percent * n' = n'
        | otherwise                 = go n' bn'
      where
        n' = n + 1
        bn' = bn + if checkBouncy n' then 1 else 0

problem_112 = solve 99

main = print problem_112
