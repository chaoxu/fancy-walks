{-# OPTIONS_GHC -O2 #-}

solve a = go 0 1 1 0
  where
    a2 = a^2

    go x am1x ap1x rmax
        | x > a2    = rmax
        | otherwise = go (x+1) (am1x * (a-1) `mod` a2) (ap1x * (a+1) `mod` a2) $ max rmax ((am1x+ap1x) `mod` a2)

problem_120 = sum $ map solve [3..1000] :: Int

main = print problem_120
