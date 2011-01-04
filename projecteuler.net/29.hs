
import Data.List

problem_29 = length . group . sort $ [a ^ b | a <- [2..100], b <- [2..100]]
