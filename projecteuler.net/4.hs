
isPalindrome :: Eq(a) => [a] -> Bool
isPalindrome a = all (\a -> ((fst a) == (snd a))) (zip a (reverse a))

range = [100..999]

problem_4 = maximum [a * b | a <- range, b <- range, isPalindrome . show $ (a * b)]
