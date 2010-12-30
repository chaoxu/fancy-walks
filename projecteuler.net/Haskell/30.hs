
import Data.List
import Data.Char

search left last = if left <= 0 then return [] else [last..9] >>= (\x -> map (x:) $ search (left - 1) x)

decode = (dropWhile (==0)) . sort . (map digitToInt) . show

check func lst = lst == decode (func lst)

f = sum . (map (^5))

answers limit = [ f lst | len <- [2..limit], lst <- search len 1, check f lst]

problem_30 = sum $ answers 10

