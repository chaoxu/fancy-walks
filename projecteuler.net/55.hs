
reverseInt :: Integer -> Integer
reverseInt = read . reverse . show

next x = x + reverseInt x

isPal x = x == reverseInt x

isLych a = not . any isPal . take 50 . tail $ iterate next a

problem_55 = length $ filter isLych [1..10000]

main = print problem_55
