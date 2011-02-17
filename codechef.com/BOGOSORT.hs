import Data.List
import Control.Monad
import Data.Array
import Data.Ratio

limit = 150

f_cache :: Array Int Integer
f_cache = array (0,limit) $ zip [0..] $ scanl (\a b -> a * toInteger b) 1 [1..limit]
f = (f_cache!)
f' n | n < 2 = 0
f' n = f (n - 2) * (n' * (n' - 1) - 2 * (n' - 1) + 1)
  where
    n' = toInteger n

solve n | n < 1 || n > limit = 0 % 1
solve n = go_cache ! n

go_cache = array (1,limit) [(i, go i) | i <- [1..limit]]
go n = (tot_ways % ways) + sum [solve target * (wys % ways) | (target, wys) <- trans]
  where
    tot_ways = f n
    trans = (0,1) : [(i, f' i * fromIntegral (n - i + 1)) | i <- [1..n-1]]
    ways = sum $ map snd trans

printRational a = putStrLn $ show (numerator a) ++ if denominator a == 1 then "" else "/" ++ show (denominator a)

main = do
    cas <- liftM read getLine
    forM_ [1..cas] $ \_ -> do
        n <- liftM read getLine :: IO Int
        printRational $ solve n 
