
import Data.Numbers.Fibonacci -- cabal install fibonacci
import Data.List
import Data.Int

modulo :: Integral a => a
modulo = 10^9

newtype ModP = ModP Int64 deriving (Eq, Show)

instance Num ModP where
    ModP a + ModP b = ModP $ (a + b) `mod` modulo
    ModP a - ModP b = ModP $ (a - b) `mod` modulo
    ModP a * ModP b = ModP $ (a * b) `mod` modulo
    fromInteger = ModP . fromIntegral . (`mod` modulo)
    abs = undefined
    signum = undefined

digits = ['1'..'9']

checkLast9 :: ModP -> Bool
checkLast9 (ModP num) = sort (show num) == digits

checkFirst9 :: Integer -> Bool
checkFirst9 num = sort (take 9 $ show num) == digits

problem_104 = head [ i :: Integer
                   | i <- [1..]
                   , checkLast9 (fib i)
                   , checkFirst9 (fib i)
                   ]

main = print $ problem_104
