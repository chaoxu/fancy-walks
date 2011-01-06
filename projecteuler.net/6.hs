
range = [1..100]

sumOfSquare = sum $ map (^2) range

squareOfSum = (^2) $ sum range

problem_6 = abs $ sumOfSquare - squareOfSum

main = print problem_6
