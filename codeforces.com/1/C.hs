
import Data.Complex
import Text.Printf

readComplex :: String -> Complex Double
readComplex = (\[x,y] -> x :+ y) . map read . words

area a b = imagPart $ a * conjugate b

angleSin a b = area a b / (magnitude a * magnitude b)

angle a b = phase (a * conjugate b)

circleR a b c = magnitude (b - c) / angleSin (b - a) (c - a) / 2

gcdR :: Double -> Double -> Double
gcdR a b 
    | a < 0 || b < 0 = gcdR (abs a) (abs b)
    | a < b = gcdR b a
    | abs b < 1e-4 = a
    | otherwise = gcdR (a-b) b

solve [a,b,c] = n' * area0
  where
    r = circleR a b c
    angA = abs (angle (b - a) (c - a)) * 2
    angB = abs (angle (c - b) (a - b)) * 2
    angC = abs (angle (a - c) (b - c)) * 2
    invN = angA `gcdR` angB `gcdR` angC
    n = 2 * pi / invN
    n' = fromIntegral $ round n
    invN' = 2 * pi / n'
    area0 = r^2 * (sin invN') / 2

main = interact $ printf "%.10f" . solve . map readComplex . take 3 . lines
