
comb :: [[Int]] -> [[Int]] -> [[Int]]
comb xs ys = [ x ++ y | x <- xs, y <- ys]

comb_n_times :: [[Int]] -> Int -> [[Int]]
comb_n_times xs 1 = xs
comb_n_times xs n = comb xs (comb_n_times xs (n-1))

repeat_combination :: [Int] -> Int -> [[Int]]
repeat_combination xs n = comb_n_times [ [x] | x <- xs] n

main :: IO ()
main = do print $ repeat_combination [1,2,3] 3
