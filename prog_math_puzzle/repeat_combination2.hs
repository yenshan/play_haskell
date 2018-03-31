
--
-- 深さ優先？幅優先？結合
-- 結合結果をもらって、さらに組み合わせ結合するやり方
--

repeat_combination :: [Int] -> Int -> [[Int]]
repeat_combination xs 0 = [[]]
repeat_combination xs n = [ x : y | x <- xs, y <- repeat_combination xs (n-1)]

main :: IO ()
main = do print $ repeat_combination [1,2,3] 3
