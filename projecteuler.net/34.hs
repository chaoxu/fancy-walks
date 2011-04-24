
import Data.Char
import Data.List

fact 0 = 1
fact n = n * fact (n - 1)

checkLength n = fact 9 * n >= 10 ^ (n - 1)

prepare 1 = map (:[]) [0..9]
prepare n = concatMap (\(x:xs) -> [y:x:xs | y <- [0..x]]) $ prepare (n-1)

calc xs = map digitToInt . sort . show . sum $ map fact xs

check xs = calc xs == xs

problem_34 = sum $ map (sum . map fact) $ filter check $ concatMap prepare $ takeWhile checkLength [2..]

main = print problem_34
