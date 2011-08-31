
import Data.List

dices = filter ((==6).length) $ subsequences [0..9]

target = [divMod (i * i) 10 | i <- [1..9]]

faceInDice x dice
    | elem x [6, 9] = elem 6 dice || elem 9 dice
    | otherwise     = elem x dice

check dice1 dice2 (x, y) = check1 || check2
  where
    check1 = faceInDice x dice1 && faceInDice y dice2
    check2 = faceInDice x dice2 && faceInDice y dice1

answer = [ (dice1, dice2)
         | dice1 <- dices
         , dice2 <- dices
         , and [check dice1 dice2 pair | pair <- target]
         ]

problem_90 = length answer `div` 2

main = print problem_90
