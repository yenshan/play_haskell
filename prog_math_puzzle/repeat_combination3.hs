

combination :: Int -> [[Int]] -> [[Int]]
combination x ys = [ x : y | y <- ys ]

repeat_combination :: [Int] -> Int -> [[Int]]
repeat_combination xs 0 = [[]]
repeat_combination xs n = concat [ combination x ys | x <- xs]
                          where
                            ys = repeat_combination xs (n-1)

main :: IO ()
main = do print $ repeat_combination [1,2,3] 3
