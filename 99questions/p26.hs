import Data.List


combinations :: Eq a => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [a : b | a <- xs, b <- combinations (n-1) (delete a xs)]

