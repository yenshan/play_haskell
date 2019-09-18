

range :: Int -> Int -> [Int]
range s e = [s..e]



range' :: Int -> Int -> [Int]
range' s e = makelist s [s]
    where
        makelist i res | i == e = res
                       | otherwise = makelist (i+1) (res++[i+1]) 

