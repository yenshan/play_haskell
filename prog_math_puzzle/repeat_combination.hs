

combination :: [Int] -> [Int] -> Int -> [[Int]]
combination xs ys 0 = [ys]
combination xs ys n = concat [combination xs (ys++[x]) (n-1) | x <- xs]

repeat_combination :: [Int] -> Int -> [[Int]]
repeat_combination xs n = combination xs [] (n+1)


main :: IO ()
main = do print $ repeat_combination [1,2,3] 3
