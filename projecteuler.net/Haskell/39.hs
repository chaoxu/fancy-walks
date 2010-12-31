
import Data.Bits
import Data.List

mysqrt :: Int -> Int
mysqrt n = helper n 1 (-1) where 
    helper n x xx = if x' == xx then min x x' else helper n x' x where x' = (x + div n x) `shiftR` 1

list limit = [ (a,b,c)
             | a <- [1 .. div limit 3]
             , b <- [1 .. div (limit-a) 2]
             , let c = mysqrt(a^2+b^2)
             , c^2 == a^2 + b^2
             , a + b + c <= limit
             ]

answer = map (\x -> (length x, head x)) $ group $ sort $ map (\(a,b,c) -> a + b + c) $ list 1000

problem_39 = snd $ maximumBy (\x y -> compare (fst x) (fst y)) $ answer

main = print problem_39

