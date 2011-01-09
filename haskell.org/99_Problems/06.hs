
isPalindrome :: Eq(a) => [a] -> Bool

isPalindrome a = all (\a -> ((fst a) == (snd a))) (zip a (reverse a))
