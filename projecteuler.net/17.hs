import Data.Char
 
one = ["","one","two","three","four","five","six","seven","eight", "nine","ten","eleven","twelve","thirteen","fourteen","fifteen", "sixteen","seventeen","eighteen", "nineteen"]
ty = ["","","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

speak x
	| x < 0 || x > 1000 = error "Out Of Range"
	| x == 0 = ""
	| x < 20 = one !! x
	| x < 100 = ty !! (x `div` 10) ++ "-" ++ speak (x `mod` 10)
	| x == 1000 = "one thousand" 
	| otherwise = one !! (x `div` 100) ++ " hundred" ++ if x `mod` 100 == 0 then "" else
		" and " ++ speak (x `mod` 100)

problem_17 = sum $ map (length.filter isLetter.speak) [1..1000]

main = print problem_17
