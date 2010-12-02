
lst = map (\r -> if (r `mod` 3) == 0 || (r `mod` 5) == 0 then r else 0) [1..999]

problem_1 = foldl (+) 0 lst
