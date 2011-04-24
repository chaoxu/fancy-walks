
import Data.List

check n = (=="123456789").sort $ mapping n

mapping n = concatMap show $ 
            last $ takeWhile (all ((==[]).tail).group.sort.concatMap show) $ 
            inits [n,n+n..]

answer :: [Integer]
answer = map (read.mapping) $ filter check [1..9999]

problem_38 = maximum answer

main = print problem_38
