
import Data.Char

f :: Integer -> Integer

f 0 = 1
f n = f (n-1) * n

problem_20 = sum . (map digitToInt) . show . f $ 100

main = print problem_20
