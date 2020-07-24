
import Data.Char


dig = ord 'z' - ord 'a' + 1

cipher_ x s = chr $ ord s + nc
    where
        nc = ((ord x - ord s) + 13) `mod` dig

cipher x 
    | 'a' <= x && x <= 'z' = cipher_ x 'a' 
    | 'A' <= x && x <= 'Z' = cipher_ x 'A' 
    | otherwise = x

rot13 str = [cipher c | c <- str]
