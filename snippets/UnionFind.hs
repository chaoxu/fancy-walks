
import Control.Monad.ST
import Data.Array.ST

type UnionFind s = STUArray s Int Int

buildUF :: (Int, Int) -> ST s (UnionFind s)
buildUF bnds = newArray bnds (-1)

findUF :: UnionFind s -> Int -> ST s Int
findUF uf a = do
    fa <- readArray uf a
    if fa == -1
      then return a
      else do
        ret <- findUF uf fa
        writeArray uf a ret
        return ret

mergeUF :: UnionFind s -> Int -> Int -> ST s Bool
mergeUF uf a b = do
    fa <- findUF uf a
    fb <- findUF uf b
    if fa == fb
      then return False
      else do
        writeArray uf fa fb
        return True

