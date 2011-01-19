
import Data.Array

inf = 10^9

solve last_row [] = last last_row
solve last_row (r:rs) = solve nr rs
  where
    r2 = zip r $ map (min inf) $ zipWith (+) last_row r
    minv a (b,c) = min (a+b) c
    nr = tail $ scanl minv inf r2

problem_81 content = solve (0 : repeat inf) matrix
  where
    matrix :: [[Int]]
    matrix = map (\x -> read $ "[" ++ x ++ "]") . lines $ content

main = readFile "input/matrix.txt" >>= print . problem_81
