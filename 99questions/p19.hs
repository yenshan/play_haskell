

rotate :: [a] -> Int -> [a]
rotate xs n = drop tn xs ++ take tn xs
    where 
        len = length xs
        tn = (len+n) `mod` len
