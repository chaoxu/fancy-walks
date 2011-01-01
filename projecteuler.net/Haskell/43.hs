{-# OPTIONS_GHC -O2 #-}

perm :: [a] -> [[a]]
perm [] = [[]]
perm (x:xs) = concatMap (insertAll x []) $ perm xs
  where
    insertAll u prev [] = [prev ++ [u]]
    insertAll u prev t@(v:vs) = (prev ++ u:t) : insertAll u (prev ++ [v]) vs

checkList = filter ((/=0).head) $ perm [0..9]

arrayToInt = foldl (\x y -> x * 10 + y) 0

list1 x = map (\n -> (x !! n * 10 + x !! (n+1)) * 10 + x !! (n+2)) [1..7]

list2 = [2,3,5,7,11,13,17]

check x = all id $ zipWith (\x y -> x `mod` y == 0) (list1 x) list2

answer = map arrayToInt $ filter check checkList

problem_43 = sum answer

--main = print problem_43
