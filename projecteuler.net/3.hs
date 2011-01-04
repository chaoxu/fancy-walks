
sieve (x:xs) = x : sieve (filter ((/=0).(`mod` x)) xs)
sieve [] = []

largestFactor n = testFactor n $ sieve [2..]

testFactor n (x:xs) 
	| x >= n           = n
	| n `mod` x /= 0   = testFactor n xs 
	| otherwise        = testFactor (n `div` x) (x:xs)

problem_3 = largestFactor (600851475143 :: Integer)


