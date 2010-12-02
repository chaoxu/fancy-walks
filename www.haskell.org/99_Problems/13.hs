import Data.List

data Element a = Multiple Int a | Single a deriving (Show)


encodeDirect :: Eq a => [a] -> [Element a]

encodeDirect = map mapping . foldr helper []
	where 
        mapping (Multiple 1 a) = Single a
        mapping a = a
        helper a [] = [Multiple 1 a]
        helper a z = 
            if a == (value $ head z)
            then (Multiple ((count $ head z)+1) (value $ head z)):(tail z)
            else (Multiple 1 a):z
        count (Multiple n a) = n
        value (Multiple n a) = a
