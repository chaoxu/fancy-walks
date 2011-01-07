
import Data.Char

problem_56 = maximum $ map (sum.map digitToInt.show) [a^b | a <- [1..100], b <- [1..100]]

main = print problem_56
