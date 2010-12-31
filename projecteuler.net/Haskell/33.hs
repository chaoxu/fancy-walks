
genList range = [(x,y) | x <- range, y <- range, x < y]

lst1 = genList [1 .. 9]
lst2 = genList [10 .. 99]



checkEq (a,b) (c,d) = a * d == b * c

checkDig a b = div b 10 == a || mod b 10 == a
removeDig a b = if div b 10 == a then b - a * 10 else (b - a) `div` 10
checkRed (a,b) (c,d) = checkDig a c && checkDig b d && removeDig a c == removeDig b d

checkTri (a,b) (c,d) = c == a * 10 && d == b * 10 || c == a * 11 && d == b * 11

answers = [b | a <- lst1, b <- lst2, checkEq a b && checkRed a b && not (checkTri a b)]

numerator = product $ map fst answers
denominator = product $ map snd answers

problem_33 = denominator `div` gcd numerator denominator

--main = print problem_33
