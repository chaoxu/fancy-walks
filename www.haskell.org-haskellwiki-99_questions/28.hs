
import Data.List

lsort :: [[a]] -> [[a]]

lsort = sortBy (\a b -> compare (length a) (length b))

lfsort z = sortBy (\a b -> compare (freq . length $ a) (freq . length $ b)) z
    where freq x = foldl (\a b -> if x == length b then a + 1 else a) 0 z
