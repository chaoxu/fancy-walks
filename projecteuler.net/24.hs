
import Data.List

f 0 = 1
f n = n * f (n - 1)

perms [] _ = []
perms xs k = x : perms (delete x xs) (k `mod` nums) where
	nums = f $ length xs - 1
	x = xs !! (k `div` nums)

problem_24 = perms "0123456789" 999999

main = putStrLn problem_24
