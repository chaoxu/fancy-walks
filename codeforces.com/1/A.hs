
get :: Integer -> Integer -> Integer
get n a = (n - 1) `div` a + 1

main = do
    line <- getLine
    let [n,m,a] = map read . words $ line
    putStrLn $ show ((get n a) * (get m a))
