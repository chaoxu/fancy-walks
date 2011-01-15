
import qualified Data.Map as M
import Data.Maybe
import Data.List

type MyMap = M.Map [Char] Int

limit = 5 :: Int

next s map = if M.member s map then M.adjust (+1) s map else M.insert s 1 map

search map upper n
    | n3 > upper = -1
    | otherwise = func $ search (next s map) upper' (n+1)
  where
    n3 = n^3 :: Integer
    s = sort $ show n3
    val = M.lookup s map
    upper' = if isJust val && fromJust val + 1 >= limit then min upper n3 else upper
    func x = if isJust val && fromJust val + 1 >= limit || x > 0 && s == sort (show x) then n3 else x

problem_62 = search M.empty (10^100 :: Integer) 1

main = print problem_62

