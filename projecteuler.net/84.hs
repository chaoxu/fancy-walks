
import Data.List
import Data.Maybe
import Data.Array
import Data.Ord
import Text.Printf

cells = [ "GO", "A1", "CC1", "A2", "T1", "R1", "B1", "CH1", "B2", "B3"
        , "JAIL", "C1", "U1", "C2", "C3", "R2", "D1", "CC2", "D2", "D3"
        , "FP", "E1", "CH2", "E2", "E3", "R3", "F1", "F2", "U2", "F3"
        , "G2J", "G1", "G2", "CC3", "G3", "R4", "CH3", "H1", "T2", "H2"
        ]

fall "G2J" = ["JAIL"]
fall cc@('C':'C':_) = "GO":"JAIL":replicate 14 cc
fall ch@('C':'H':_) = "GO":"JAIL":"C1":"E3":"H2":"R1":nextR:nextR:nextU:back3:replicate 6 ch
  where
    nextRU = case ch of 
        "CH1" -> ("R2", "U1")
        "CH2" -> ("R3", "U2")
        "CH3" -> ("R1", "U1")
    nextR = fst nextRU
    nextU = snd nextRU

    back3 = cells !! ((pos - 3) `mod` length cells)
    pos = fromJust (elemIndex ch cells)

fall xs = [xs]

move (doubles,cell) = [ ((ndoubles, ncell'), 1.0 / fromIntegral (length nexts * length falled) :: Double)
                      | (ndoubles, ncell) <- nexts
                      , let falled = fall ncell
                      , ncell' <- falled
                      ]
  where
    n = 4

    pos = fromJust (elemIndex cell cells)

    nexts = [ if ndoubles == 3 then (0,"JAIL") else (ndoubles, ncell)
            | dice1 <- [1..n]
            , dice2 <- [1..n]
            , let ndoubles = if dice1 == dice2 then doubles + 1 else 0
            , let ncell = cells !! ((pos + dice1 + dice2) `mod` length cells)
            ]

moveCached = (cache!)
  where
    bnds = ((0,0),(2,length cells - 1))

    cache = listArray bnds [map norm $ move (i, cells !! j) | (i, j) <- range bnds]

    norm ((a, b), c) = ((a, fromJust $ elemIndex b cells), c)
    

transform prob = prob'
  where
    bnds = ((0,0),(2,length cells - 1))

    lst = [ (nstate, p * p2)
          | (state,p) <- prob
          , (nstate, p2) <- moveCached state
          ]

    prob' = assocs $ accumArray (+) 0.0 bnds lst

apply n = foldl1 (.) . replicate n

probFinal = [sum [fromJust $ lookup (j,i) prob | j <- [0..2]] | i <- [0..length cells - 1]]
  where
    prob = iterate transform [((0,0), 1.0)] !! 512

cellNum :: String -> String
cellNum cell = printf "%02d" pos
  where
    pos = fromJust (elemIndex cell cells)

problem_84 = concatMap (cellNum . fst) $ take 3 $ reverse $ sortBy (comparing snd) $ zip cells probFinal

main = putStrLn problem_84
