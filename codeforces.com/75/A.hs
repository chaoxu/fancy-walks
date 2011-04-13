
main = interact $ solve . map read . lines

solve :: [Int] -> String
solve (a:b:_) = if a' + b' == c' then "YES" else "NO"
  where
    c = a + b
    a' = convert a
    b' = convert b
    c' = convert c

convert :: Int -> Int
convert = read . filter (/='0') . show
