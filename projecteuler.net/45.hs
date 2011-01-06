{-# OPTIONS_GHC -O2 #-}

--func must be strictly increasing taking [1..] as input
checkFunc :: Ord b => (Integer -> b) -> b -> Bool
checkFunc func n = helper 1 (head $ dropWhile ((<=n).func) [1..])
  where
    helper l r | l == r = func l == n
    helper l r = if func m >= n then helper l m else helper (m+1) r
      where
        m = (l + r) `div` 2

funcT n = n * (n + 1) `div` 2
funcP n = n * (3 * n - 1) `div` 2
funcH n = n * (2 * n - 1)

answersTH = filter (\n -> checkFunc funcT n && checkFunc funcH n) $ [1..20]

diffSeq :: Num a => [a] -> [a]
diffSeq [] = error "diffSeq: list must be non-empty"
diffSeq [v] = []
diffSeq (x:xs) = (head xs - x) : diffSeq xs

polySeq :: Num a => [a] -> [a]
polySeq [] = error "polySeq: list must be non-empty"
polySeq [v] = cycle [v]
polySeq seq = scanl (+) (head seq) $ polySeq $ diffSeq seq

problem_45 = head $ filter (checkFunc funcP) $ dropWhile (<=40755) $ polySeq answersTH

main = print $ problem_45
