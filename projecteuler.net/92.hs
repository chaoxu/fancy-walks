
import Data.Array.IArray

next 0 = 0
next x = next a + b ^ 2
  where
    (a, b) = x `divMod` 10

lst89 = (89:) $ takeWhile (/=89) $ tail $ iterate next 89

limit = 10 ^ 7 - 1

canArrive89 = (cache!)
  where
    bnds = (1, limit)
    cache = listArray bnds $ map go $ range bnds :: Array Int Bool

    go 1 = False
    go x
        | elem x lst89 = True
        | otherwise    = canArrive89 (next x)

problem_92 = length [n | n <- [1..limit], canArrive89 n]

main = print problem_92
