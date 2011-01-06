
import Data.Char

mapping f n = foldl (\x y -> x * 10 + y) 0 $ lst ++ f (reverse lst)
	where lst = map digitToInt $ show n

gen xs = map (mapping tail) xs ++ map (mapping id) xs

numbers = gen [1..999]

check2 n = rep2 n == reverse (rep2 n)
	where
		rep2 0 = []
		rep2 x = mod x 2 : rep2 (div x 2)
		
answer = filter check2 numbers

problem_36 = sum answer

main = print problem_36
