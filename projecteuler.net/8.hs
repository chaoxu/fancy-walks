
import Data.Char
import IO

largest :: [Char] -> Int
largest (a:(xs@(b:c:d:e:_))) = max (product $ map digitToInt (a:b:c:d:e:[])) $ largest xs
largest _ = 0

problem_8 = largest . filter isDigit

main = readFile "input/p8.txt" >>= print . problem_8
