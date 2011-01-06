{-# OPTIONS_GHC -O2 #-}

import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B

main = do
    (nk:lines) <- fmap B.lines B.getContents
    let [n,k] = map (fst . fromJust . B.readInt) $ B.words nk
    let querys = take n $ map (fst . fromJust . B.readInt) lines
    print $ length $ filter ((==0).(`mod` k)) querys
