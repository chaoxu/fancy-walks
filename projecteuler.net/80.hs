
import Data.Char

isqrt :: Integer -> Integer
isqrt n = fst . head $ dropWhile (\(a,b) -> a + 1 /= b && a /= b) $ iterate (\(_,x) -> (x, (x + div n x) `div` 2)) (-1, 1)

sqrt100 n = if digits `mod` 10^110 == 0 then 0 else sum $ map digitToInt $ take 100 $ show digits
  where
    digits = isqrt (n * 10^220)

problem_80 = sum $ map sqrt100 [1..100] -- maybe [0..99] ??

main = print problem_80
