
import Data.Array
import Data.List

inf = 10^9

solve last_row [] = minimum last_row
solve last_row (r:rs) = solve nr' rs
  where
    r2 = zip r $ map (min inf) $ zipWith (+) last_row r
    minv a (b,c) = min (a+b) c
    nr = tail $ scanl minv inf r2
    nr' = init $ scanr (flip minv) inf $ zip r nr

problem_82 content = solve (repeat 0) matrix
  where
    matrix :: [[Int]]
    matrix = transpose $ map (\x -> read $ "[" ++ x ++ "]") . lines $ content

main = readFile "input/matrix.txt" >>= print . problem_82
