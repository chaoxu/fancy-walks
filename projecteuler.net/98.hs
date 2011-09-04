{-# OPTIONS_GHC -O2 #-}

import Data.Function
import Data.List
import Data.Bits
import Data.Int
import Data.Maybe
import Data.Char

isSq :: Int64 -> Bool
isSq n = sqrtn * sqrtn == n
  where
    func x = (x + n `div` x) `shiftR` 1

    sqrtn = go 1 (func 1)

    go x y | x == y || x + 1 == y = x
    go x y = go y (func y)
    

solvePair :: (String, String) -> [(Int64, Int64)]
solvePair (a, b)
    | length chars > 10 = []
    | otherwise         = [(x, y) | (x, y) <- numPairs, isSq x && isSq y]
  where
    chars = map head $ group $ sort a
    numPairs = [ (trans a, trans b)
               | seq <- enumerate (length chars)
               , let mapping = zip chars seq
               , let func ch = fromJust $ lookup ch mapping
               , func (head a) /= 0
               , func (head b) /= 0
               , let trans = read . map (intToDigit.func)
               ]

enumerate :: Int -> [[Int]]
enumerate n = go n [0..9]
  where
    go 0 set = [[]]
    go dep set = concat [map (x:) $ go (dep-1) (delete x set) | x <- set]

problem_98 text = maximum $ map fst answers ++ map snd answers
  where
    pairs = [(x, y) | lst <- ws', x <- lst, y <- lst, x < y]
    ws' = filter ((>=2).length) $ groupBy ((==) `on` sort) . sortBy (compare `on` sort) $ ws
    ws = read $ "[" ++ text ++ "]" :: [String]

    answers = concatMap solvePair pairs

main = readFile "input/words.txt" >>= print . problem_98
