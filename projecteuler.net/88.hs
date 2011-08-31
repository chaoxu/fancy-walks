
import Data.List
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Control.Applicative
import qualified Data.IntSet as Set

primes = sieve [2..]
  where
    sieve (x:xs) = x : sieve (filter ((/=0).(`mod`x)) xs)

smallestFactor x = head [arr ! x | arr <- cache, inRange (bounds arr) x]
  where
    go (x:xs) a
        | x * x > a      = a
        | a `mod` x == 0 = x
        | otherwise      = go xs a

    lst = map (go primes) [1..]

    cache = [ listArray (1, n) lst :: UArray Int Int
            | n <- map (2^) [10..]
            ]

factors 1 = []
factors n = s : factors (n `div` s)
  where
    s = smallestFactor n

divisors n = sort ds
  where
    fs = factors n
    ds = map product $ sequence $ map (scanl (*) 1) (group fs)

divisorPartition n = go n n
  where
    go 1 _ = return []
    go x maxv = do
        d <- filter (/=1) $ takeWhile (<=maxv) (divisors x)
        (d:) <$> go (x `div` d) d

pairs = [(n, product a - sum a + length a) | n <- [2..], a <- divisorPartition n]

target = [2..12000]

problem_88 = go pairs Set.empty (Set.fromList target)
  where
    go ((n,k):xs) ans set
        | Set.null set     = sum $ Set.toList ans
        | Set.member k set = go xs (Set.insert n ans) (Set.delete k set)
        | otherwise        = go xs ans set

main = print problem_88
