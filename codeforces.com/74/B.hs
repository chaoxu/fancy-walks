
import Control.Applicative

data Direction = HEAD
               | TAIL
    deriving (Show, Eq)

simulate n sto con dir turn seqs
    | con == sto       = "Controller " ++ show turn
    | seqs == []       = "Stowaway"
    | head seqs == '1' = simulate n targetPos nextCon nextDir (turn + 1) (tail seqs)
    | otherwise        = simulate n nextSto   nextCon nextDir (turn + 1) (tail seqs)
    where
        targetPos = if nextDir == HEAD then n else 1
        (nextCon, nextDir) = case dir of
            HEAD -> if con == 1 then (con + 1, TAIL) else (con - 1, HEAD)
            TAIL -> if con == n then (con - 1, HEAD) else (con + 1, TAIL)
        nextSto = case compare sto con of
            LT -> if sto == 1 then sto else sto - 1
            GT -> if sto == n then sto else sto + 1

main = do
    [n,sto,con] <- map read . words<$> getLine :: IO [Int]
    [_,dirS] <- words <$> getLine
    let dir = if dirS == "head" then HEAD else TAIL
    seqs <- getLine
    putStrLn $ simulate n sto con dir 0 seqs
