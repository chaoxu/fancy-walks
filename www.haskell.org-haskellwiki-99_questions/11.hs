import Data.List

data Element a = Multiple Int a | Single a deriving (Show)


encodeModified :: Eq a => [a] -> [Element a]

encodeModified = map helper . group
	where 
		helper [x] = Single x
		helper xs = Multiple (length xs) (head xs)

