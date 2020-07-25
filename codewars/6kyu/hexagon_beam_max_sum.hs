module Kata (maxHexagonBeam) where

import Data.List


takeNs [] _ = []        
takeNs (x:xs) ys = take x ys : takeNs xs (drop x ys)

paddZero n xs = xs ++ replicate (n-length xs) 0
    

padd n xs = left ++ right
    where
        left = map (paddZero (n+n-1)) $ take n xs
        right = reverse $ map (reverse.paddZero (n+n-1).reverse) $ take (n-1) (reverse xs)

hexagon n lst = takeNs nums (concat $ repeat lst)
    where
        maxl = n + (n-1)
        ns = [n..maxl-1]
        nums = ns ++ [maxl] ++ reverse ns


maxsum xs = maximum [sum x | x <- xs]


maxHexagonBeam :: Int -> [Int] -> Int
maxHexagonBeam n lst = maximum [maxsum beam1, maxsum beam2, maxsum beam3]
    where
        beam1 = hexagon n lst
        beam2 = transpose $ padd n beam1
        beam3 = transpose $ map reverse $ padd n (map reverse beam1)
        

