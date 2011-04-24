
nums :: String -> [Integer]
nums = map read . words

problem_13 = take 10 . show . sum . nums

main = readFile "input/p13.txt" >>= putStrLn . problem_13 
