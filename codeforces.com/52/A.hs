{-# OPTIONS_GHC -O2 #-}

import Data.Char

count :: Eq a => a -> [a] -> Int
count t = foldl (\x y -> if y == t then x + 1 else x) 0 

solve arr = (a + b + c) - (a `max` b `max` c)
  where
    a = count '1' arr
    b = count '2' arr
    c = count '3' arr

main = do
    line <- getLine
    arr <- fmap (filter isDigit) getLine
    print $ solve arr

