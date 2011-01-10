
import Control.Monad

solve es [] = [(e, 0) | e <- es]
solve es (q:qs) = [(e, find e) | e <- es, e /= q]
  where
    solve' = solve es qs
    find e = minimum [v + if e == e' then 0 else 1 | (e', v) <- solve']
main = do
    cases <- liftM read getLine
    forM [1..cases] $ \cas -> do
        engines <- (liftM read getLine >>= flip replicateM getLine)
        querys <- (liftM read getLine >>= flip replicateM getLine)
        putStrLn $ "Case #" ++ show cas ++ ": " ++ show (minimum $ map snd $ solve engines querys)
