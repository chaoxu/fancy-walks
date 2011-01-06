
powMod :: Integer -> Integer -> Integer -> Integer
powMod m _ 0 = 1
powMod m a p = (`mod` m) $ if (p `mod` 2 == 1) then x*x*a else x*x
  where
    x = powMod m a (p `div` 2)

m = 10^10

norm str | length str > 10 = norm $ tail str
norm str | length str < 10 = norm $ '0':str
norm str = str

problem_48 = norm $ show $ sum $ map (\n -> powMod m n n) [1..1000]

main = putStrLn $ problem_48
