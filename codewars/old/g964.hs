
import Debug.Trace

nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p = nbYear_ p0 1
    where
      nbYear_ pp n | pop >= p = n
                   | otherwise = nbYear_ pop (n+1)
        where
           a = truncate $ (fromIntegral pp) * percent/100
           pop = pp + a + aug 

