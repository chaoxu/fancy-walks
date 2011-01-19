
import Data.Graph
import Data.Char
import Data.List

-- observation: the graph is a DAG

problem_79 content = intersect walk digits
  where
    edges = concatMap ((\[x,y,z] -> [(x,y),(y,z)]) . map digitToInt) . words $ content
    walk = map intToDigit . topSort . buildG (0, 9) $ edges
    digits = intersect "0123456789" content

main = readFile "input/keylog.txt" >>= putStrLn . problem_79
