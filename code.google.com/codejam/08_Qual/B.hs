
import Control.Monad
import Data.List

readTime :: [Char] -> Int
readTime str = hour * 60 + min
  where
    hour = read $ take 2 str
    min = read $ drop 3 str

solve la lb = loop sa sb
  where
    sa = sort la
    sb = sort lb
    loop a [] = 0
    loop [] b = length b
    loop a@(ha:ta) b@(hb:tb)
        | hb >= ha = loop ta tb
        | otherwise = loop a tb + 1

main = do
    cases <- liftM read getLine
    forM_ [1..cases] $ \cas -> do
        turnTime <- liftM read getLine :: IO Int
        [n,m] <- liftM (map read . words) getLine :: IO [Int]
        a <- replicateM n $ liftM (map readTime . words) getLine
        b <- replicateM m $ liftM (map readTime . words) getLine
        let ansa = solve (map ((+turnTime).(!!1)) b) (map head a)
        let ansb = solve (map ((+turnTime).(!!1)) a) (map head b)
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show ansa ++ " " ++ show ansb
