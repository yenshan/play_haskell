import Data.List

{-
    - NG answer
combinations :: Eq a => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [a : b | a <- xs, b <- combinations (n-1) (delete a xs)]
-}

subset :: [a] -> [[a]]
subset [] = [[]]
subset (x:xs) = [ x : a | a <- subset xs] ++ subset xs

combinations :: Int -> [a] -> [[a]]
combinations n xs = [ a | a <- subset xs, length a == n]



combinations' :: Int -> [a] -> [[a]]
combinations' 1 xs = [[a] | a <- xs]
combinations' n xs | n == length xs = [xs]
combinations' n (x:xs) = [x : a | a <- combinations' (n-1) xs] ++ (combinations' n xs)


