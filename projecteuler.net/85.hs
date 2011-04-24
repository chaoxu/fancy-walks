
import Data.Ord
import Data.List
import Data.Maybe

target = 2000000

problem_85 = (\(x,y) -> find x * find y) $ snd $ minimumBy (comparing $ abs . (target-) . fst) $ solve nums (reverse nums)
  where
    nums = [n * (n + 1) `div` 2 | n <- [1..2000]]
    find n = 1 + fromJust (elemIndex n nums)
    solve [] _ = []
    solve _ [] = []
    solve a@(x:xs) b@(y:ys) = 
        if xy > target 
        then (xy,(x,y)) : solve a ys
        else (xy,(x,y)) : solve xs b
      where
        xy = x * y

main = print problem_85
