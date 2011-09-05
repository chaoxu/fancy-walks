
-- nearly optimum solution works

answer6 = [11, 18, 19, 20, 22, 25]

problem_103 = concat $ map (show.(+answer6!!3)) $ 0 : answer6

main = putStrLn $ problem_103
