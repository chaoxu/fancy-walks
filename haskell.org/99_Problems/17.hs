
split :: [a] -> Int -> ([a],[a])

split x = splitHelper ([], x)
    where 
        splitHelper (x, []) _ = (x, [])
        splitHelper (left, right) num = 
            if num <= 0 
            then (left, right)
            else splitHelper (left ++ [head right], tail right) (num - 1)
