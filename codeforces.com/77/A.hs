
import Data.Array
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad

findId name = fromJust $ elemIndex name names
  where
    names = ["Anka", "Chapay", "Cleo", "Troll", "Dracul", "Snowy", "Hexadecimal"]

parseLine line = (findId (ws !! 0), findId (ws !! 2))
  where
    ws = words line

solve :: Array (Int,Int) Bool -> [Int] -> (Int, Int)
solve arr exp = minimum $ map score seps
  where
    r = [0..6]
    seps :: [[[Int]]]
    seps = [ [x \\ y, y, r \\ x]
           | x <- subsequences r
           , x /= r && x /= []
           , y <- subsequences x
           , y /= x && y /= []
           ]
    score sep = (maximum scores - minimum scores, negate . sum $ map likes sep)
      where
        scores = zipWith div exp $ map length sep
    likes grp = length [True | a <- grp , b <- grp, arr ! (a, b)]

main = do
    n <- read <$> getLine :: IO Int
    likes <- replicateM n (parseLine <$> getLine)
    let arr = accumArray (||) False ((0,0),(6,6)) $ map (\x -> (x,True)) likes
    exp <- map read . words <$> getLine :: IO [Int]
    let (minv, maxv) =  solve arr exp
    putStrLn $ show minv ++ " " ++ show (negate maxv)
