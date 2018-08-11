--
--https://www.hackerrank.com/challenges/ctci-bubble-sort/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=sorting
--
import Data.List

swap (x:xs) ys cnt
    | xs == [] = (ys ++ [x],cnt)
    | x > head xs = swap (x : tail xs) (ys ++ [head xs]) (cnt + 1)
    | otherwise = swap xs (ys ++ [x]) cnt

bubbleSortLoop xs n cnt
    | n <= 0 = (xs,cnt)
    | otherwise = bubbleSortLoop ys (n - 1) (cnt + n)
                  where
                    (ys,n) = swap xs [] 0

bubbleSort xs = bubbleSortLoop xs (length xs) 0

main = do
        n <- readLn :: IO Int
        aTemp <- getLine
        let a = Data.List.map (read :: String -> Int) . words $ aTemp
        let (xs,cnt) = bubbleSort a
        putStrLn $ "Array is sorted in " ++ show cnt ++ " swaps."
        putStrLn $ "First Element: " ++ show (head xs)
        putStrLn $ "Last Element: " ++ show (last xs)
