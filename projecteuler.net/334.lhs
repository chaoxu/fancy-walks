% Project Euler, Problem 334
% Bin Jin
%

Problem Statement
-----------------

In Plato's heaven, there exist an infinite number of bowls in a straight line.
Each bowl either contains some or none of a finite number of beans.
A child plays a game, which allows only one kind of move: removing two beans from any bowl, and putting one in each of the two adjacent bowls.
The game ends when each bowl contains either one or no beans.

Consider now $1500$ adjacent bowls containing $b_1, b_2, \cdots, b_{1500}$ beans respectively, all other bowls being empty. Find how many moves it takes before the game ends.

Analysis
--------

An intuitional observation is that, the game will end at last. So maybe there are some value determined by game states(a function),
which increasing (or decreasing) strictly between moves and have a upperbound (or lowerbound). And it's easy to find one:
let the position of $i$-th bean be $p_i$, $F(p) = \sum_{i=1}^{n}\sum_{j=i+1}^n (p_j - p_i)$ is one.

Here is some illustration:

+ suppose now we are splitting a bowl with $b+2$ beans, and there are $a$ beans on the left and $c$ beans on the right.
+ suppose we move the $(a+1)$-th and $(a+b+2)$-th bean.
+ move $(a+1)$-th bean to the left bowl, $F(p)$ increased $(b+1+c)-a$.
+ move $(a+b+2)$-th bean to the right bowl, $F(p)$ increased $(a+b+1)-c$.
+ so $F(p)$ increased $2b+2$.

And $F(p)$ have an upperbound, and it's easy to prove this: $p_{i+1}-p_i\leq 2$ at every moment.

Another two observation:

+ $\sum p_i$ is constant.
+ there are at most one empty bowls in the final state. (consider bowls from the leftmost bean to the rightmost)
    * suppose there are two empty bowls
    * all bowls (if exists) between these two bowls contains exactly one bean.
    * to make an empty bowl in a consecutive nonempty bowls, the bowl should contains exactly two beans.
    * one of the two empty bowl must contains exactly two bowls before the final state
    * move two beans back to this bowl, the number of (non-empty) bowls between two empty bowls decreased one
    * It's impossible that two empty bowls is adjacent

With this two observation, the final state could be determined.

+ suppose $l(p)$ is the position of the leftmost bean, and $r'(p)$ for the rightmost one
+ let $r(p)=r'(p)$ is there are some empty bowl between $l(p)$ and $r'(p)$, or $r(p)=r'(p)+1$ otherwise
+ $r(p) = l(p) + n$
+ $\sum p_i = \sum_{i=l(p)}^{r(p)}i - e(p)$, where $e(p)$ s.t. $l(p)<e(p)\leq r(p)$ is the postion of the empty bowl.
+ $\sum p_i = \frac{(l(p)*2+n)(n+1)}{2} - l(p) - e'(p)$, where $e'(p)=e(p)-l(p) \in [1,n]$
+ $\sum p_i = l(p)n-e'(p)+\frac{n(n+1)}{2}$, note that $n$ is given, and $e'(p)$ lies in $[1,n]$, calculating $e'(p)$ is just a modulo operation.

Now problem is, given the start state and final state, how to calculate the number of steps? 
The $b$ in $b+2$ is bothering, so we make $b=0$ by adding beans one by one.

Preparation
-----------

`generateB` is defined in the original problem statement.
we left `solve :: [Int] -> Integer` as our core function.

> {-# OPTIONS_GHC -O2 #-} -- run with +RTS -K256m -A32m -RTS
> import Data.List
> import Data.Int
> import Data.Bits
> 
> main = print problem_344
>
> problem_344 = solve $ generateB 1500
>
> generateB :: Int -> [Int]
> generateB n = take n $ map (\t -> t `mod` (2^11) + 1)$ tail seqT
>   where
>     seqT = iterate genT 123456
>     genT :: Int -> Int
>     genT n = if even n then n2 else n2 `xor` 926252
>       where
>         n2 = n `div` 2
>

State Definition
----------------

here we define a state `Bowls` to describe a tuple $l(p), r(p), e(p)$, which is a final state. 
and a function `addBeanToBowl :: Bowls -> Integer -> (Bowls, Integer)` to add one bean to a particular bowl and 
return the next final position and number of moves to achieve it.

the definition of `Bowls`, `lef` is $l(p)$, `rig` is $r(p)$, `emp` is $e(p)$

> data Bowls = Bowls 
>            { lef  :: Integer
>            , rig  :: Integer
>            , emp  :: Integer
>            }
>     deriving (Show, Eq, Ord)

define an empty `Bowls`

> emptyBowls :: Bowls
> emptyBowls = Bowls 0 0 0

`sumP` calculate $\sum p_i$

> sumP :: Bowls -> Integer
> sumP (Bowls l r e) = fromIntegral (l + r) * fromIntegral (r - l + 1) `div` 2 - e

`sumMargin` calculate $F(p)$, ignore emp first, and remove margins related with emp.

+ `allMargin n` is $\sum_{i=0}^{n-1}\sum_{j=i+1}^{n-1}(j-i) = \frac{n(n^2-1)}{6}$
+ `singleMargin n` is $\sum_{i=0}^{n-1} (i - 0) = \frac{n(n-1)}{2}$

> sumMargin :: Bowls -> Integer
> sumMargin (Bowls l r e) = allMargin (r - l + 1) - singleMargin (e - l + 1) - singleMargin (r - e + 1)
>
> allMargin n = n * (n^2 - 1) `div` 6
> singleMargin n = n * (n - 1) `div` 2

finally `addBeanToBowl bowls pos` to add a bean to `pos` bowl of `bowls`, it's assumed that `pos` lies in the range
of `bowls`. we first calculate `sump` and `sump`, and find the result `Bowls` first. as on every steps, $F(p)$ increased
2(on any moment, $b=0$), we can find the number of steps easily.

> addBeanToBowl :: Bowls -> Integer -> (Bowls, Integer)
> addBeanToBowl bowls@(Bowls l r e) pos 
>     | pos < l || pos > r  = error "addBeanToBowl: out of range, "
>     | steps2 `mod` 2 == 1 = error "addBeanToBowl: steps2 not a multiple of 2, "
>     | otherwise           = (finalState, steps2 `div` 2)
>   where
>     sump = sumP bowls + pos
>     summ = sumMargin bowls + singleMargin (pos - l + 1) + singleMargin (r - pos + 1) - abs (pos - e)
>     finalState = solveBowls (r - l + 1) sump
>     steps2 = sumMargin finalState - summ

the only left work is `solveBowls n sump`, it's indeed a modulo operation, as we have seen before.

> solveBowls :: Integer -> Integer -> Bowls
> solveBowls n sump = Bowls l (l + n) (l + e)
>   where
>     value = sump - n * (n + 1) `div` 2 -- equals to l * n - e
>     e = n - value `mod` n
>     l = (value + e) `div` n

Solving
-------

now just chain this up

>
> solve :: [Int] -> Integer
> solve xs = snd $ foldl iter (emptyBowls, 0) ps
>   where
>     ps = concat $ zipWith replicate xs [0..]
>     iter (st, pmoves) p = (st', pmoves + moves)
>       where
>         (st', moves) = st `addBeanToBowl` p
