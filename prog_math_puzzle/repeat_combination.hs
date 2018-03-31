
--
-- 深さ優先結合
-- 結合結果を引数に渡して、繰り返し結合するやり方
--

combination :: [Int] -> [Int] -> Int -> [[Int]]
combination xs ys 0 = [ys]
combination xs ys n = concat [combination xs (ys++[x]) (n-1) | x <- xs]

repeat_combination :: [Int] -> Int -> [[Int]]
repeat_combination xs n = combination xs [] n


main :: IO ()
main = do print $ repeat_combination [1,2,3] 3
