
import Data.List

maxDegree = 16

func :: Integer -> Integer
func n = 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10

makeSeq :: Num a => [a] -> [a]
makeSeq [] = repeat 0
makeSeq xs = scanl (+) (head xs) $ makeSeq (zipWith (-) (tail xs) xs)

solve seq = sum [ head [x | (x, y) <- zip seq2 seq', x /= y]
                | xs <- inits seq'
                , xs /= []
                , let seq2 = take maxDegree $ makeSeq xs
                , seq2 /= seq'
                ]
  where
    seq' = take maxDegree seq

problem_101 = solve $ map func [1..]

main = print problem_101
