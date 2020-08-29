




merge :: [Int] -> [Int]
merge xs = nums ++ (replicate (length xs - length nums) 0)
     where
        doMerge [] = []
        doMerge [x] = [x]
        doMerge (x:y:xs) | x == 0 = doMerge (y:xs)
                         | x == y = (x+y):(doMerge xs)
                         | otherwise = x:(doMerge (y:xs))
        nums = doMerge $ filter (/=0) xs
