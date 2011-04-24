
import Data.Char

digits = concatMap show [1..]

problem_40 = product $ map (digitToInt . (digits!!) . \x -> 10^x - 1) [0..6]

main = print problem_40
