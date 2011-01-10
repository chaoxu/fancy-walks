
import Control.Monad

main = do
    cases <- liftM read getLine
    forM [1..cases] $ \_ -> do
        [a,b] <- liftM (map read.words) getLine
        print $ gcd a b
