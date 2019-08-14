

slice :: [a] -> Int -> Int -> [a]
slice xs i k = [a | (a,idx) <- indexed xs, i <= idx, idx <= k]
    where
        indexed xs = zip xs [1..(length xs)]
