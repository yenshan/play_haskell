
--
-- 幅優先結合
-- 結合結果を引数に渡して、繰り返し方式。末尾最適可能。
--

combination :: [Int] -> [[Int]] -> Int -> [[Int]]
combination xs ys 0 = ys
combination xs ys n = combination xs [ x : y | x <- xs, y <- ys ] (n-1)

repeat_combination :: [Int] -> Int -> [[Int]]
repeat_combination xs n = combination xs [[]] n

main :: IO ()
main = do print $ repeat_combination [1,2,3] 3
