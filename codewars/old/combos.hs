
import Data.List


combo n 1 = [[n]]
combo n l 
    | n == l = [[ 1 | _ <- [1..n]]]
    | (n-l) == 1 = [[ 1 | _ <- [1..l-1]] ++ [2]]
    | l == 2 = [ [x, n-x] | x <- [1..n `div` 2]]
    | otherwise = nub $ concat [ map sort $ map (x:) $ combo (n-x) (l-1) | x <- [1..n-l]]

combos n = concat $ map (combo n) [1..n]

main = do
        print $ combos 20

