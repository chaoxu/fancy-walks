import System.Random

rnd_select :: [a] -> Int -> IO [a]

rnd_select _ 0 = return []

rnd_select (x:xs) n = 
    do 
        r <- randomRIO (0, length xs)
        if r < n 
            then do 
                remaining <- rnd_select xs (n-1)
                return (x:remaining)
            else rnd_select xs n

diff_select n m
    | n > m     = error "too few elements"
    | otherwise = rnd_select [1..m] n
