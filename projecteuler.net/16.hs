
import Data.Char

problem_16 = sum $ map digitToInt $ show $ 2^1000

main = print problem_16
