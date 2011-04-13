
import Control.Applicative
import Control.Monad
import Data.List
import Data.Function
import qualified Data.Map as M
import Data.Maybe

parseLine :: String -> (String, String, Int)
parseLine line = (head sp, init $ init target, score)
  where
    sp = words line
    target = head $ filter (elem '\'') sp
    score = case head (sp !! 1) of
        'p' -> 15
        'c' -> 10
        'l' -> 5

type MyMap = M.Map String Int

solve :: String -> [(String, String, Int)] -> [String]
solve myName pairs = filter (/=myName) . map fst . reverse . sortBy (compare `on` snd) . reverse .  sortBy (compare `on` fst) $ M.assocs mp
  where
    incr :: MyMap -> (String, String, Int) -> MyMap
    incr mp (f,r,s) = mp2
      where
        mp1 = M.insertWith' (+) f (if r == myName then s else 0) mp
        mp2 = M.insertWith' (+) r (if f == myName then s else 0) mp1
    mp = foldl incr M.empty pairs

main = do
    myName <- getLine
    n <- read <$> getLine :: IO Int
    pairs <- replicateM n (parseLine <$> getLine)
    let sorted = solve myName pairs
    mapM_ putStrLn sorted
