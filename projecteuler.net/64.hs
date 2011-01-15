
import qualified Data.Map as M
import Data.List
import Data.Maybe

isqrt :: Integer -> Integer
isqrt n = fst . head $ dropWhile (\(a,b) -> a + 1 /= b && a /= b) $ iterate (\(_,x) -> (x, (x + div n x) `div` 2)) (-1, 1)

next :: Integer -> (Integer,(Integer,Integer)) -> (Integer,(Integer,Integer))
next n (_,(a,b)) = (z,(a',b'))
  where
    sqrtn = isqrt n
    z = quot (sqrtn + a) b
    --   \frac{b}{\sqrt{n}+(a-zb)}
    -- = \frac{\sqrt{n}-(a-zb)}{(n-(a-zb)^2)/b}
    a' = -(a - z * b)
    b' = div (n-a'^2) b

cf n = iterate (next n) (0,(0,1))

period = helper M.empty 0 
  where
    helper map num (x:xs) = if isNothing val then helper map' (num+1) xs else num - fromJust val
      where
        val = M.lookup x map
        map' = M.insert x num map 

isSquare n = sqrtn^2 == n
  where
    sqrtn = isqrt n

problem_64 = length $ filter odd $ map (period. cf) $ filter (not.isSquare) [2..10000]

main = print problem_64
