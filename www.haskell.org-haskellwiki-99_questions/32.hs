
myGCD :: Integer -> Integer -> Integer

myGCD a b 
    | a < 0 = myGCD (-a) b
    | b < 0 = myGCD a (-b)
myGCD a 0 = a
myGCD a b = myGCD b (a `rem` b)
