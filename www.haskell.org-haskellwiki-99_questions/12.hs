import Data.List

data Element a = Multiple Int a | Single a deriving (Show)

decodeModified :: Eq a => [Element a] -> [a];

decodeModified = foldr helper [] 
    where 
        helper d [] = myExpand d
        helper d z = (myExpand d) ++ z
        myExpand (Single a) = [a]
        myExpand (Multiple 0 a) = []
        myExpand (Multiple n a) = a : myExpand (Multiple (n-1) a)

