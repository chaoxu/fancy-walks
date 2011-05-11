data BoundedInteger = Infinite Bool | Finite Integer deriving Eq

instance Show BoundedInteger where
    show (Infinite True) = "Postive Infinite"
    show (Infinite False) = "Negative Infinite"
    show (Finite a) = show a

instance Num BoundedInteger where
    Infinite v + _ = Infinite v -- +oo + -oo ? aha, I'm crazy
    _ + Infinite v = Infinite v
    Finite a + Finite b = Finite (a + b)

    Finite 0 * _ = 0
    _ * Finite 0 = 0
    Infinite v * a = Infinite (v == (a > 0))
    a * Infinite v = Infinite (v == (a > 0))
    Finite a * Finite b = Finite (a * b)

    negate (Infinite v) = Infinite (not v)
    negate (Finite v) = Finite (negate v)

    signum (Infinite True) = 1
    signum (Infinite False) = -1
    signum (Finite a) = Finite $ signum a

    abs (Infinite _) = Infinite True
    abs (Finite v) = Finite (abs v)

    fromInteger = Finite

instance Ord BoundedInteger where
    Finite a `compare` Finite b = compare a b
    Finite _ `compare` Infinite v = if v then LT else GT
    Infinite v `compare` Finite _ = if v then GT else LT
    Infinite a `compare` Infinite b
        | a == b    = EQ
        | a         = GT
        | otherwise = LT

instance Bounded BoundedInteger where
    minBound = Infinite False
    maxBound = Infinite True
