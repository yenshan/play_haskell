import Data.List

solve :: String -> String
solve xs = let ys = filterSame xs
           in if ys == xs then xs else solve ys

filterSame [] = []
filterSame [x] = [x]
filterSame (x:y:xs) | x == y = filterSame (xs)
                    | otherwise = x : filterSame (y:xs)
