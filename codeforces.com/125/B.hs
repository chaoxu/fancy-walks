main = interact (solve 0)

solve d ('>':xs) = ">\n"++solve d xs
solve d ('<':'/':xs) = replicate ((d-1)*2) ' ' ++ "</" ++ solve (d - 1) xs
solve d ('<':xs) = replicate (d*2) ' ' ++ "<" ++ solve (d + 1) xs
solve d (x:xs) = x : solve d xs
solve _ [] = ""
