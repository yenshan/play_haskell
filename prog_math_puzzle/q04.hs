import Data.List

cut :: Int -> [Int]
cut x = [(x `quot` 2), (x `quot` 2)+(x `mod` 2)]


cut_nth :: [Int] -> Int -> [Int]
cut_nth [] _ = []
cut_nth xs 0 = xs
cut_nth (x:xs) n
    | x <= 1 = x : cut_nth xs n
    | otherwise = cut x ++ cut_nth xs (n-1)

cut_bar_iter :: [Int] -> Int -> Int -> Int
cut_bar_iter xs np cnt
    | all (==1) xs = cnt
    | otherwise = cut_bar_iter ys np (cnt+1) 
                  where
                    ys = reverse (sort (cut_nth xs np))

cut_bar :: Int -> Int -> Int
cut_bar len np = cut_bar_iter [len] np 0

main :: IO ()
main = do
        print $ cut_bar 20 3
        print $ cut_bar 100 5
