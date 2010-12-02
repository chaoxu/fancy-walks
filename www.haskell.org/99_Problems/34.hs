
totient 1 = 1
totient n = foldl (\z cur -> if 1 == gcd cur n then z + 1 else z) 0 [1..(n-1)]
