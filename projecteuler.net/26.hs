
import Data.List
import Data.Maybe

clear25 n | n `mod` 2 == 0 = clear25 (n `div` 2)
clear25 n | n `mod` 5 == 0 = clear25 (n `div` 5)
clear25 n = n

try9 1 _ _ = 0
try9 n m9 cnt | m9 `mod` n == 0 = cnt
try9 n m9 cnt = try9 n (m9 * 10 + 9) (cnt + 1)

cycleLen n = try9 (clear25 n) 9 1

arr = map cycleLen [1..999]

problem_26 = (+1).fromJust $ elemIndex (maximum arr) arr

