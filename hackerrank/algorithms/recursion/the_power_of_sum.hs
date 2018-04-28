--
-- https://www.hackerrank.com/challenges/the-power-sum/problem
--
subset :: [Int] -> Int -> [[Int]]
subset [] _ = [[]]
subset (x:xs) max = let ss = [s | s <- subset xs max, sum s <= max]
                    in ss ++ map (x:) ss

pow_seq :: Int -> [Int]
pow_seq n = [a ^ n| a <- [1..]]

pow_of_sum :: Int -> Int -> Int
pow_of_sum x n = let pows = takeWhile (<=x) (pow_seq n)
                 in length [b | b <- subset pows x, sum b == x]

main :: IO ()
main = do
        x_t <- getLine
        let x = read $ x_t :: Int
        n_t <- getLine
        let n = read $ n_t :: Int
        print $ pow_of_sum x n
