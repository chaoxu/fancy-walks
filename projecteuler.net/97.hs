
modulo :: Integer
modulo = 10^10

newtype ModP = ModP Integer deriving Eq

instance Show ModP where
    show (ModP a) = show a

instance Num ModP where
    ModP a + ModP b = ModP $ (a + b) `mod` modulo
    ModP a - ModP b = ModP $ (a - b) `mod` modulo
    ModP a * ModP b = ModP $ (a * b) `mod` modulo
    fromInteger = ModP . (`mod` modulo) . fromInteger
    abs = undefined
    signum = undefined

problem_97 = 28433 * 2 ^ 7830457 + 1 :: ModP

main = print problem_97
