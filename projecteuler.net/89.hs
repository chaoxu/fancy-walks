
import Data.Maybe

rules = [ ("DCCCC", "CM")
        , ("CCCC", "CD")
        , ("LXXXX", "XC")
        , ("XXXX", "XL")
        , ("VIIII", "IX")
        , ("IIII", "IV")
        ]

startWith a b = take (length b) a == b

normalize [] = []
normalize text = case text' of
    Just t  -> normalize t
    Nothing -> head text : normalize (tail text)
  where
    text' = listToMaybe [y ++ drop (length x) text | (x, y) <- rules, text `startWith` x]

problem_89 text = sum (map length ws) - sum (map length ws')
  where
    ws = words text
    ws' = map normalize ws

main = readFile "input/roman.txt" >>= print . problem_89
