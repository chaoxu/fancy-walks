
problem_9 = head [a * b * (1000 - a - b) | a <- [1..333], b <- [(a+1)..1000], a^2 + b^2 == (1000 - a - b)^2, b < 1000 - a - b]

main = print problem_9
