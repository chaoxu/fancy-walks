
import IO

nums :: String -> [Integer]
nums = map read . words

problem_13 = take 10 . show . sum . nums

main = do
    file <- openFile "input/p13.txt" ReadMode
    input <- hGetContents file
    putStrLn $ problem_13 input
