

import Data.List

-- f 0 = 1, f 1 = 1, f 2 = 2, f 3 = 4, f n = f (n - 1) + f (n - 2) + f (n - 3) + f (n - 4)

f = [1,1,2,4] ++ zipWith4 (\x y z w -> x+y+z+w) f (tail f) (drop 2 f) (drop 3 f)

problem_117 = f !! 50

main = print problem_117
