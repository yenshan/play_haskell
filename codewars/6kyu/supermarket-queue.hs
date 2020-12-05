
queueTime :: [Int] -> Int -> Int
queueTime q t = maximum $ foldl (putCustomer) tq q 
    where
        tq = replicate t 0


putCustomer [] _ = []
putCustomer tq@(x:xs) n | x == min = (x+n) : xs 
                        | otherwise = x : putCustomer xs n                       where
        min = minimum tq 


test  = queueTime [2,2,3,3,4,4] 2


