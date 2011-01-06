
import IO

grid :: String -> [Integer]
grid rawString = map read $ words rawString

grid2 gr x y | x < 0 || x >= 20 || y < 0 || y >= 20 = 0
grid2 gr x y = gr !! (x * 20 + y)

directions = [(1,0),(0,1),(1,1),(1,-1)]

findAnswer gr2 (x,y) (dx,dy) = product [gr2 (x + dx * k) (y + dy * k) | k <- [0..3]]

problem_11 input = maximum [findAnswer (grid2 $ grid input) (x,y) dir | x <- [0..19], y <- [0..19], dir <- directions]

main = do
    file <- openFile "input/p11.txt" ReadMode
    input <- hGetContents file
    print $ problem_11 input
