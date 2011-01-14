
import Data.Function
import Data.List
import Data.Maybe

ranks = "23456789TJQKA"
findRank = (+2) . fromJust . (flip elemIndex ranks)

data Card = Card 
          { rank :: Int
          , suit :: Char
          } deriving (Eq)

instance Ord Card where
    compare = compare `on` rank

instance Show Card where
    showsPrec _ (Card r s) = showChar (ranks !! r) . showChar s

readCard :: String -> Card
readCard str = Card { rank = findRank (head str), suit = str !! 1 }

type Player = [Card]

score :: Player -> (Int, [Int])
score p 
    | flush && straight && rank (head s) == 10 = (10, rankBySize)
    | flush && straight = (9, rankBySize)
    | (lens !! 0) == 4 = (8, rankBySize)
    | (lens !! 0) == 3 && (lens !! 1) == 2 = (7, rankBySize)
    | flush = (6, rankBySize)
    | straight = (5, rankBySize)
    | (lens !! 0) == 3 = (4, rankBySize)
    | (lens !! 0) == 2 && (lens !! 1) == 2 = (3, rankBySize)
    | (lens !! 0) == 2 = (2, rankBySize)
    | otherwise = (1, rankBySize)
  where
    s = sort p 
    flush = isFlush s
    straight = isStraight s
    groupByRank = groupBy ((==) `on` rank) $ reverse s
    groupBySize = sortBy (flip compare `on` length) groupByRank
    lens = map length groupBySize
    rankBySize = map (rank.head) groupBySize
    isFlush [b] = True
    isFlush (a:b) = (suit a == suit (head b)) && isFlush b
    isStraight a = rank (a !! 0) + 4 == rank (a !! 4) && length lens == 5

solve line = if score a > score b then 1 else 0
  where
    (a, b) = splitAt 5 $ map readCard $ words line

problem_54 = sum . map solve . lines

main = readFile "input/poker.txt" >>= print . problem_54
