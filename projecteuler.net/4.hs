
isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = all (uncurry (==)) (zip a (reverse a))

range = [100..999]

problem_4 = maximum [a * b | a <- range, b <- range, isPalindrome . show $ (a * b)]

main = print problem_4
