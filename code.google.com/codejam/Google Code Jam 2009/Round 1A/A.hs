{-# OPTIONS_GHC -O2 #-}

import Data.List
import Data.Maybe
import Data.Char
import Data.Array
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
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Graph

parseInput = do 
    cas <- readInt
    replicateM cas $ do
        line <- BS.unpack <$> readLine
        return $ map read $ words line
  where
    readInt = state $ fromJust . BS.readInt . BS.dropWhile isSpace
    readInteger = state $ fromJust . BS.readInteger . BS.dropWhile isSpace
    readString = state $ BS.span (not . isSpace) . BS.dropWhile isSpace
    readLine = state $ BS.span (not . isEoln) . BS.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'

main = do
    input <- evalState parseInput <$> BS.getContents
    forM_ (zip [1..] input) $ \(cas, params) -> do
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (solve params)

trans :: Integral a => a -> a -> a
trans base num = sum $ map (^2) lst
  where
    lst = unfoldr (\n -> if n == 0 then Nothing else Just (n `mod` base, n `div` base)) num

isNice (base, num)
    | num > limit2 = go2 (base, num)
    | otherwise    = cache ! (base, num)
  where
    limit = 9^2 * 10 -- upperbound of a tranformed value
    limit2 = 100000  -- buffer to speed up
    bnds = ((2, 1), (10, limit2))
    cache = listArray bnds [if snd idx <= limit then go idx else go2 idx | idx <- range bnds]

    go (base, num) = loop (f num) (g num)
      where
        f = trans base
        g = f . f
        loop 1 _ = True
        loop x y | x == y = False
        loop x y = loop (f x) (g y)

    go2 (base, num)
        | num == 1        = True
        | otherwise       = isNice (base, trans base num)

bitContain :: Bits a => a -> a -> Bool
bitContain a b = (complement a .&. b) == 0

solve bases = minimum . map snd $ filter (flip bitContain bits.fst) prepared
  where
    bits = sum $ map (\x -> 2^(x-2)) bases
    prepared = [(5,2),(7,17),(13,5),(15,25),(21,6),(23,695),(29,229),(31,533),(37,311),(39,191),(45,401),(47,713),(53,2807),(55,4859),(61,7307),(63,2207),(69,8),(71,77),(77,309),(79,545),(85,216),(87,4557),(93,1975),(95,4137),(101,6211),(103,6351),(109,1001),(111,5719),(117,120785),(119,47089),(125,48769),(127,58775),(133,385),(135,3),(141,95),(143,81),(149,1135),(151,707),(157,415),(159,1695),(165,9721),(167,2159),(173,125),(175,143),(181,2753),(183,68091),(189,43465),(191,37131),(197,553),(199,3487),(205,2893),(207,27),(213,29627),(215,10089),(221,4977),(223,23117),(229,24855),(231,35785),(237,6393),(239,128821),(245,710761),(247,662619),(253,1026657),(255,569669),(261,10),(263,13),(269,19),(271,23),(277,44),(279,1277),(285,469),(287,79),(293,8787),(295,167),(301,7),(303,10309),(309,5527),(311,6307),(317,49),(319,7895),(325,97),(327,623),(333,2455),(335,219),(341,608),(343,37079),(349,40285),(351,3879),(357,44265),(359,51909),(365,34527),(367,120407),(373,285357),(375,697563),(381,245035),(383,2688153),(389,397),(391,739),(397,1009),(399,91),(405,1033),(407,12399),(413,4577),(415,6073),(421,3137),(423,1337),(429,29913),(431,120149),(437,71735),(439,613479),(445,218301),(447,711725),(453,7961),(455,12085),(461,4417),(463,1177),(469,9867),(471,47775),(477,108161),(479,28099),(485,142901),(487,48041),(493,246297),(495,346719),(501,2662657),(503,4817803),(511,11814485)] :: [(Int,Int)]
