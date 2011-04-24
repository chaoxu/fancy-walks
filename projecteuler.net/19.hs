
isleap year = year `mod` 4 == 0 && year `mod` 100 /= 0 || year `mod` 400 == 0

daysInMonth year month 
	| month == 2                     = if isleap year then [1..29] else [1..28]
	| month `elem` [1,3,5,7,8,10,12] = [1..31]
	| otherwise                      = [1..30]

days = [(year, month, day) | year <- [1900..2000], month <- [1..12], day <- daysInMonth year month]

problem_19 = length [(year,month,day) | (x,(year,month,day)) <- zip (cycle [1..7]) days, x == 7 && year >= 1901 && day == 1]

main = print problem_19
