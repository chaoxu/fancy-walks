import Char
import List
import Data.Function
main = interact$show.solve.groupBy((==)`on`isDigit).head.words
solve xs | head (head xs)`elem`"*/" = 0
         | otherwise                = head $ go xs
go [x:_] | isDigit x = repeat 1
go ((x:y):z)
  | isDigit x        = go z
  | any(`elem`"*/")y = repeat 0
  | otherwise        = map (`mod`1000003) $ scanl1 (+) $ drop (length y+1) $ go z
go _ = repeat 0
