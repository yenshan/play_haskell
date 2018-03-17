
filtera :: Int -> [Int] -> [Int]
filtera n [] = []
filtera n (x:xs)
    | x < n = [x] ++ filtera n xs
    | otherwise = filtera n xs

main :: IO ()
main =  do 
            n_tmp <- getLine
            let n = read n_tmp :: Int
            ar_tmp <- getContents 
            let ar = map read (lines ar_tmp) :: [Int]
            putStr . unlines $ map show (filtera n ar)

