{-# OPTIONS_GHC -O2 #-}

import Data.Maybe
import Data.Array
import Data.Int
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B

class State a where
    add :: a -> a -> a
    trans :: a -> a -> a

data MyState = MyState 
             { minv :: Int64
             , delta :: Int64
             }

instance State MyState where
    add lb rb = MyState (minv lb `min` minv rb) 0
    trans fa son = MyState (minv son + delta fa) (delta fa)

type Interval = (Int,Int)

midpoint :: Interval -> Int
midpoint (l,r) = (l + r) `div` 2

data State a => SegTree a = Branch Interval a (SegTree a) (SegTree a)
                          | Leaf a

getState :: State a => SegTree a -> a
getState (Leaf v) = v
getState (Branch _ v _ _) = v

build :: State a => Interval -> (Int -> a) -> SegTree a
build (l,r) func | l == r = Leaf (func l)
build inter@(l,r) func = Branch inter val lb rb
  where
    m = midpoint inter
    lb = build (l,m) func
    rb = build (m+1,r) func
    val = getState lb `add` getState rb

query :: State a => SegTree a -> Int -> Int -> a
query (Leaf v) _ _ = v
query (Branch inter v lb rb) l r 
    | inter == (l,r) = v
    | r <= m = trans v $ query lb l r
    | l > m = trans v $ query rb l r
    | otherwise = trans v $ add (query lb l m) (query rb (m+1) r)
  where
    m = midpoint inter

modify :: State a => SegTree a -> Int -> Int -> (a -> a) -> SegTree a
modify (Leaf v) _ _ func = Leaf (func v)
modify (Branch inter v lb rb) l r func
    | inter == (l,r) = Branch inter (func v) lb rb
    | r <= m = Branch inter (trans v $ add lbs (getState rb)) lb' rb 
    | l > m = Branch inter (trans v $ add (getState lb) rbs) lb rb' 
    | otherwise = Branch inter (trans v $ add lbs rbs) lb' rb' 
  where
    m = midpoint inter
    lb' = modify lb l (min r m) func
    rb' = modify rb (max (m+1) l) r func
    lbs = getState lb'
    rbs = getState rb'

runline :: Int -> SegTree MyState -> [Int] -> IO (SegTree MyState)
runline n tree [lf,rg] = do
    let ans = minv $ if lf <= rg then query tree lf rg else query tree lf (n-1) `add` query tree 0 rg
    print ans
    return tree
runline n tree [lf,rg,dl] | lf > rg = runline n tree [lf,n-1,dl] >>= \tree' -> runline n tree' [0,rg,dl]
runline n tree [lf,rg,dl] = return $ modify tree lf rg helper
  where
    helper st = MyState (minv st + fromIntegral dl) (delta st + fromIntegral dl)

myRead = fst . fromJust . B.readInt

main = do
    (ns:ais:ms:qs) <- fmap (B.lines) B.getContents
    let n = myRead ns
    let m = myRead ms
    let ai = map myRead $ B.words ais
    let arrai = array (0,n-1) $ zip [0..] ai
    let !tree0 = build (0,n-1) (\n -> MyState (fromIntegral$arrai!n) 0)
    let !querys = map (map myRead . B.words) $ take m qs
    foldM (runline n) tree0 querys
