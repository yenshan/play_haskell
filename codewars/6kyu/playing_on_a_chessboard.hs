
import Data.Ratio

game' n = if dt == 1 then Left nt else Right (nt, dt)
    where
        ns = replicate n [1..n]
        ds = [[m..m+(n-1)] | m <- [2..n+1]]
        sums = sum $ zipWith (%) (concat ns) (concat ds)
        dt = denominator sums
        nt = numerator sums

game n = if dt == 1 then Left nt else Right (nt, dt)
    where
        sums = (sum $ [1..n] ++ [1..(n-1)]) % 2
        dt = denominator sums
        nt = numerator sums