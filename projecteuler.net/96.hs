{-# OPTIONS_GHC -O2 #-}

import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Bits
import Data.Word
import Data.Ord
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad
import Data.Function

import Debug.Trace

cells = 81

rowID x = x `div` 9
colID x = x `mod` 9
blockID x = (rowID x `div` 3) * 3 + (colID x `div` 3)

conflict x y = rowID x == rowID y || colID x == colID y || blockID x == blockID y

conflictsWith = (cache!)
  where
    cache = listArray (1, cells) [[y | y <- [1..cells], x /= y && conflict (x-1) (y-1)] | x <- [1..cells]] :: Array Int [Int]

-- bit 1 .. bit 9 : masks
-- bit 12 .. bit 15 : bitCount of masks
type Opinions = Word16

toChar msk = case [i | i <- [1..9], msk `testBit` i] of
    []  -> 'E'
    [x] -> chr (x + ord '0')
    _   -> '.'

maskOpinions = 0x03fe :: Word16

fromMask :: Word16 -> Opinions
fromMask msk = msk' .|. (countBits msk' `shiftL` 12)
  where
    msk' = msk .&. maskOpinions

countBits :: Word16 -> Word16
countBits a0 = a4
  where
    a1 = (a0 .&. 0x5555) + ((a0 .&. 0xaaaa) `shiftR` 1)
    a2 = (a1 .&. 0x3333) + ((a1 .&. 0xcccc) `shiftR` 2)
    a3 = (a2 .&. 0x0f0f) + ((a2 .&. 0xf0f0) `shiftR` 4)
    a4 = (a3 .&. 0x00ff) + ((a3 .&. 0xff00) `shiftR` 8)

removeMask :: Opinions -> Word16 -> Opinions
removeMask a b = fromMask (a .&. complement b)

data Grid = Grid { grid :: UArray Int Opinions, fixed :: UArray Int Bool }

instance Show Grid where
    show g = map toChar (elems $ grid g)

select :: Grid -> (Int, Int) -> Maybe Grid
select g (pos, color)
    | not preRequire = Nothing
    | fixed g ! pos  = Just g
    | otherwise      = Just $ Grid grid' fixed'
  where
    preRequire = (grid g ! pos) `testBit` color 
    fixed' = fixed g // [(pos, True)]
    msk = 1 `shiftL` color
    grid' = accum removeMask (grid g) ((pos, complement msk) : [(pos', msk) | pos' <- conflictsWith pos])

initGrid :: Grid
initGrid = Grid (listArray (1, cells) (repeat $ fromMask maskOpinions)) (listArray (1, cells) (repeat False))

selectCell :: Grid -> Maybe Int
selectCell g
    | null lst  = Nothing
    | otherwise = Just $ minimumBy (comparing (grid g!)) lst
  where
    lst = [i | (i, fixi) <- assocs (fixed g), not fixi]

prune2 :: Grid -> Maybe Grid
prune2 g
    | null updates = return g
    | otherwise    = return $ Grid grid' (fixed g)
  where
    lst = groupBy ((==) `on` snd) $ sortBy (comparing snd) $ filter ((==2).(`shiftR`12).snd) $ assocs (grid g)
    updates = concat [ [(pos, msk) | pos <- sameBlock]
                     | sameMask <- lst
                     , (i, (x, msk)) <- zip [0..] sameMask
                     , (y, _) <- take i sameMask
                     , conflict (x-1) (y-1)
                     , let sameBlock = conflictsWith x `intersectOrd` conflictsWith y
                     ]
    grid' = accum removeMask (grid g) updates

intersectOrd :: Ord a => [a] -> [a] -> [a]
intersectOrd [] x = []
intersectOrd x [] = []
intersectOrd a@(x:xs) b@(y:ys) = case compare x y of 
    EQ -> x : intersectOrd xs ys
    LT -> intersectOrd xs b
    GT -> intersectOrd a ys

search :: Grid -> Maybe Grid
search g = case selectCell g of 
    Nothing -> return g
    Just p  -> msum [select g (p, col) >>= prune2 >>= search | let msk = grid g ! p, col <- [1..9], msk `testBit` col]

solve :: String -> Maybe Grid
solve str = foldM select initGrid lst >>= prune2 >>= search
  where
    lst = [(i, ord fi - ord '0') | (i, fi) <- zip [1..] str, elem fi ['1'..'9']]

problem_96 = sum.map (read.take 3.show.fromMaybe (error "unsolvable puzzle").solve.filter(not.isSpace).concat.tail).chop 10.lines

chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

main = readFile "input/sudoku.txt" >>= print . problem_96
