
try (p,q) n (a,b) (c,d) = 
    if bd > n then (a,b) else
        if ac * q < p * bd 
            then try (p,q) n (ac,bd) (c,d)
            else try (p,q) n (a,b) (ac,bd)
  where
    ac = a + c
    bd = b + d

problem_71 = fst $ try (3,7) 1000000 (0,1) (1,1)

main = print problem_71
