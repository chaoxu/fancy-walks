
import qualified Data.Sequence as DS

next prime state = next' state DS.empty
  where
    head' (h DS.:< _) = h
    tail' (_ DS.:< t) = t
    next' (x:xs) set = prepared : next' xs nset
      where
        prepared = x + if DS.length set == prime then head' (DS.viewl set) else 0
        set' = set DS.|> prepared
        nset = if DS.length set == prime then tail' (DS.viewl set') else set'

primes = sieve [2..]
  where
    sieve (x:xs) = x : sieve (filter ((/=0).(`mod` x)) xs)

problem_77 = length $ takeWhile (<5000) ans
  where
    func = foldl1 (.) $ map next $ take 100 primes
    ans = func $ 1 : repeat 0

main = print problem_77
