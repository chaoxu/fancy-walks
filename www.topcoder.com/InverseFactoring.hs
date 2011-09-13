module InverseFactoring where 

getTheNumber :: [Int] -> Int
getTheNumber factors = maximum factors * minimum factors
