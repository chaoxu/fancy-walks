
problem_1 = sum $ filter (\r -> (r `mod` 3) == 0 || (r `mod` 5) == 0) [1..999]
