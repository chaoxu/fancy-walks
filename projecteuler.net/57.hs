
func (a,b) = (b'+a',a')
  where
    (a',b') = (a+b,b)

lst = take 1000 $ iterate func (3,2)

check (a,b) = length (show a) > length (show b)

problem_57 = length $ filter check lst

main = print problem_57
