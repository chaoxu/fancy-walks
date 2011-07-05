{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe
import Data.Char
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Int
import Data.Ratio
import Data.Bits
import Data.Function
import Data.Ord
import Control.Monad.State
import Control.Monad
import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Graph
import Control.Parallel.Strategies
import Text.Printf

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        itemNum <- readInt
        storeNum <- readInt
        gasPrice <- readInt
        itemNamesWithInfo <- replicateM itemNum readString
        let itemNames = map (BS.filter isLetter) itemNamesWithInfo
        let itemInfo = map (\x -> BS.last x == '!') itemNamesWithInfo
        storeInfo <- replicateM storeNum $ do
            pos <- (,) <$> readInt <*> readInt
            remaining <- readLine
            let words = filter (not . BS.null) $ BS.words remaining
            let items = [(fromJust $ elemIndex itemName itemNames, read (BS.unpack price) :: Int) | word <- words, let [itemName, price] = BS.split ':' word]
            return (pos, items)
        return (gasPrice, itemInfo, storeInfo)
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    input <- evalState parseInput <$> BS.getContents
    let output = parMap rdeepseq solve input
    forM_ (zip [1..] output) $ \(cas, result) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ printf "%.7f" result

solve :: (Int, [Bool], [((Int,Int),[(Int, Int)])]) -> Double
solve (gasPrice, itemInfo, storeInfo) = minCost (0,0,False)
  where
    itemNum = length itemInfo
    storeNum = length storeInfo + 1
    positions = (0,0) : map fst storeInfo
    itemInStore = listArray (0, storeNum - 1) $ [(-1,0)] : map snd storeInfo :: Array Int [(Int,Int)]
    itemInfoArr = listArray (0, itemNum - 1) itemInfo :: UArray Int Bool
    mask = 1 `shiftL` itemNum - 1

    dist = listArray ((0, 0), (storeNum-1, storeNum-1)) [ sqrt $ fromIntegral ((x1-x2)^2+(y1-y2)^2)
                                                        | (x1, y1) <- positions
                                                        , (x2, y2) <- positions
                                                        ] :: UArray (Int,Int) Double

    minCost :: (Int, Int, Bool) -> Double
    minCost = (cache!)
      where
        bnds = ((0,0,False),(storeNum-1,mask,True))
        cache = listArray bnds $ map go $ range bnds :: Array (Int, Int, Bool) Double

        go (pos, msk, flag)
            | msk == mask && pos == 0 = 0
            | otherwise               = minimum answers
          where
            answers = [ minCost target + dis * fromIntegral gasPrice + fromIntegral cost
                      | npos <- [0..storeNum - 1]
                      , not flag || npos == 0 || npos == pos
                      , npos /= 0 || pos /= 0
                      , (toBuy, cost) <- itemInStore ! npos
                      , toBuy == -1 || (msk .&. (1 `shiftL` toBuy)) == 0
                      , let target = if toBuy == -1 then (npos, msk, False) else (npos, msk .|. (1 `shiftL` toBuy), itemInfoArr ! toBuy || pos == npos && flag)
                      , let dis = dist ! (npos, pos)
                      ]
