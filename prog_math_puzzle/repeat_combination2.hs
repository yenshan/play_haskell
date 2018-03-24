
comb :: [[Int]] -> [[Int]] -> [[Int]]
comb xs ys = [ x ++ y | x <- xs, y <- ys]

comb_n_times :: [[Int]] -> Int -> [[Int]]
comb_n_times xs 1 = xs    
comb_n_times xs n = comb xs (comb_n_times xs (n-1))


repeat_combination :: [Int] -> Int -> [[Int]]
repeat_combination xs 1 = [xs]
repeat_combination xs n = let ys = [[x] | x <- xs] 
                          in comb_n_times ys n

main :: IO ()
main = do print $ repeat_combination [1,2,3] 3
