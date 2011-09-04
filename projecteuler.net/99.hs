
import Data.Ord
import Data.List
import Data.Maybe

solve str = log a * b
  where
    pos = fromJust $ elemIndex ',' str
    a = read $ take pos str :: Double
    b = read $ drop (pos + 1) str :: Double

problem_99 text = index
  where
    values = map solve $ take 1000 $ lines text
    index = fst $ maximumBy (comparing snd) $ zip [1..] values

main = readFile "input/base_exp.txt" >>= print . problem_99
