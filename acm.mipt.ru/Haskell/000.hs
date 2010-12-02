import Data.List
import Data.Char

solve s =
    let sp = map read $ words s
    in show $ (sp !! 0) + (sp !! 1)

main = interact $ unlines.map solve.lines

