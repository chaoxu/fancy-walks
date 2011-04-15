
import Control.Applicative
import Control.Monad
import Data.List
import Data.Function

parseLine :: String -> (String, Int)
parseLine str = (head ws, plus * 100 - minus * 50 + sum probs)
  where
    ws = words str
    plus = read $ ws !! 1 :: Int
    minus = read $ ws !! 2 :: Int
    probs = map read $ drop 3 ws :: [Int]

main = do
    n <- read <$> getLine :: IO Int
    cs <- replicateM n (parseLine <$> getLine)
    putStrLn $ fst $ maximumBy (compare `on` snd) cs
