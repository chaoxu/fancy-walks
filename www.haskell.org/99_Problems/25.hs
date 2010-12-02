import System.Random

insertAt :: a -> [a] -> Int -> [a]

insertAt ch [] 1 = [ch]
insertAt _ [] _ = []
insertAt ch (x:xs) pos = 
    if pos == 1
    then ch:remaining
    else remaining
    where remaining = x:(insertAt ch xs (pos - 1))

rnd_permu :: [a] -> IO [a]

rnd_permu [] = return []

rnd_permu (x:xs) = 
    do
        remaining <- rnd_permu xs
        pos <- randomRIO(1, 1 + length xs)
        return $ insertAt x remaining pos
