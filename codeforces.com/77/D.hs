
import Control.Monad
import Control.Applicative
import Data.Array
import Data.Int

type Block = [Bool]

checkRotate :: Block -> (Bool, Bool)
checkRotate block
    | sz == 2 || sz == 3 = if head block then (False, True) else (True, False)
    | sz == 6            = if block !! 1 then (False, True) else (True, False)
    | otherwise          = (True, True)
  where
    sz = length $ filter id block

buildBlocks :: Int -> Int -> [[Char]] -> Array (Int,Int) Block
buildBlocks rows cols str = array bnds [((x,y), getBlock x y) | x <- [0..rows-1] , y <- [0..cols-1]]
  where
    bndsC = ((0, 0), (rows * 4, cols * 4))
    arr = array bndsC [((x,y),ch) | (x, line) <- zip [0..] str, (y, ch) <- zip [0..] line]
    bnds = ((0,0), (rows-1,cols-1))
    getBlock x y = [ch == 'O' | i <- [0..2], j <- [0..2], let ch = arr ! (bx + i, by + j)]
      where
        bx = x * 4 + 1
        by = y * 4 + 1

solve :: Int -> Int -> Array (Int, Int) (Bool, Bool) -> Int
solve rows cols rotates = solveCols (map solveOneCol [0..cols-1]) (map solveTwoCol [0..cols-1])
  where
    modulo = 10^9+7
    myPlus :: Int -> Int -> Int
    myPlus a b = if sum >= modulo then sum - modulo else sum
      where
        sum = a + b
    myMul :: Int -> Int -> Int
    myMul a b = fromIntegral (mul `mod` fromIntegral modulo)
      where
        mul = (fromIntegral a) * (fromIntegral b) :: Int64
    solveOneCol col
        | odd rows = 0
        | otherwise = if and [fst $ rotates ! (x, col) | x <- [0..rows-1]] then 1 else 0
    solveTwoCol col = if col + 1 < cols then solveCol $ listArray (0, rows-1) thisCol else 0
      where
        thisCol = [ (a && c, b && d)
                  | x <- [0..rows-1]
                  , let (a,b) = rotates ! (x,col)
                  , let (c,d) = rotates ! (x,col+1)
                  ]
    solveCol col = foldl myPlus 0 [go (x+1) | x <- takeWhile canTwo' $ filter even [0..rows-1], canOne x]
      where
        bnds = (0, rows)
        memo = array bnds [(x, go x) | x <- range bnds]
        go r
            | r == rows = 1
            | otherwise = sum1 `myPlus` sum2
          where
            go' r = memo ! r
            sum1 = if canOne r then go' (r + 1) else 0
            sum2 = if canTwo r then go' (r + 2) else 0
        canOne r = r < rows && snd (col ! r)
        canTwo r = r + 1 < rows && fst (col ! r) && fst (col ! (r + 1))
        canTwo' r = r == 0 || canTwo (r - 2)
    solveCols col1 col2 = go 0
      where
        bnds = (0, cols)
        arr1 = listArray bnds col1
        arr2 = listArray bnds col2
        memo = array bnds [(x, go x) | x <- range bnds]
        go c
            | c == cols = 1
            | otherwise = sum1 `myPlus` sum2
          where
            go' c = memo ! c
            sum1 = (arr1 ! c) `myMul` go' (c + 1)
            sum2 = if c + 1 < cols then (arr2 ! c) `myMul` go' (c + 2) else 0

main = do
    [rows,cols] <- map read . words <$> getLine :: IO [Int]
    blocks <- buildBlocks rows cols <$> replicateM (rows * 4 + 1) getLine
    print $ solve rows cols (checkRotate <$> blocks)
