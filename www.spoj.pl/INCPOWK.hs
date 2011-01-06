{-# OPTIONS_GHC -O2 #-}

import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Control.Monad

getBits :: Integer -> [Bool]
getBits 0 = []
getBits a = (a `mod` 2 == 1) : getBits (a `div` 2)

restoreBits :: Integer -> [Bool] -> Integer
restoreBits base [] = 0
restoreBits base (x:xs) = restoreBits base xs * base + if x then 1 else 0

readInt = fst . fromJust . BS.readInt
readInteger = fst . fromJust . BS.readInteger

main = do
    (cass:querys) <- liftM BS.lines BS.getContents
    let cas = readInt cass
    let qs = map (map readInteger . BS.words) $ take cas querys
    forM_ qs (\[k,n] -> print $ restoreBits k $ getBits n)
        
