{-# OPTIONS_GHC -O2 #-}

func :: Integer -> Integer
func n = n * (3 * n - 1) `div` 2

pentagonal = map func [1..]

isPentagonal n = helper 1 (head $ dropWhile ((<=n).func) lst)
  where
    lst = map (2^) [0..]
    helper l r | l == r = func l == n
    helper l r = if func m >= n then helper l m else helper (m+1) r
      where
        m = (l + r) `div` 2

checkD d = any (\n -> isPentagonal (n + d) && isPentagonal (n + d + n)) $ take 5000 pentagonal -- FIXME: find proper way to handle upper_bound

problem_44 = head $ filter checkD pentagonal

--main = print $ problem_44
