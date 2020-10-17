module LeastLarger (leastLarger) where


import Data.List


leastLarger :: [Int] -> Int -> Maybe Int
leastLarger xs i = if l==[] then Nothing else elemIndex (head l) xs
    where
        l = sort $ filter (>xs!!i) xs
 

main = do 
        print $ leastLarger [4, 1, 3, 5, 6] 4 
        print $ leastLarger [4, 1, 3, 5, 6] 0 
