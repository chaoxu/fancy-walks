
import Data.List
import Data.Ratio
import Data.Map (Map, (!))
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Data.Ord

import Debug.Trace

digits = [0..9] :: [Int]

perms = [perm | perm <- subsequences digits, length perm <= 4 && not (null perm)]

calculate :: [Int] -> Set (Ratio Int)
calculate = (cache!)
  where
    cache = Map.fromList [(perm, go perm) | perm <- perms]

    go [x] = Set.singleton (x % 1)
    go xs = Set.fromList [ av `op` bv
                         | a <- subsequences xs
                         , let b = xs \\ a
                         , a /= [] && b /= []
                         , av <- Set.toList (calculate a)
                         , bv <- Set.toList (calculate b)
                         , (idx, op) <- zip [1..] [(+),(-),(*),(/)]
                         , idx < 4 || bv /= 0
                         ]

maxConsecutive perm = case takeWhile inAnswers [1..] of
    [] -> 0
    xs -> last xs
  where
    answers = calculate perm
    inAnswers n = Set.member (n % 1) answers

problem_93 = fst $ maximumBy (comparing snd) lst
  where
    lst = [ (map intToDigit perm, maxConsecutive perm)
          | perm <- perms
          , length perm == 4
          ]

main = putStrLn problem_93
