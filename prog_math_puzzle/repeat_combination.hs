
combination :: [Int] -> Int -> [Int] -> [[Int]]
combination _ 0 res = [res]
combination xs n res = concat [ combination xs (n - 1) (res ++ [x]) | x <- xs]

repeat_combination :: [Int] -> Int -> [[Int]]
repeat_combination xs 1 = [xs]
repeat_combination xs n = combination xs n []

main :: IO ()
main = do print $ repeat_combination [1,2,3] 3
