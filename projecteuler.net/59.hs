
import Data.List
import Data.Char
import Data.Ord
import Data.Bits

main = readFile "input/cipher1.txt" >>= print . problem_59

problem_59 content = solve (read ("[" ++ content ++ "]") :: [Int])

decrypt cipher key = map chr $ zipWith xor cipher (cycle key)

getPrintable cipher = filter (all isPrint) [decrypt cipher [a,b,c] | let range = [97..122], a <- range, b <- range, c <- range]

solve = sum . map ord . maximumBy (comparing (length.filter isLetter)).getPrintable
