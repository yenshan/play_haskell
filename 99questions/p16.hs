

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [a | (a, i) <- indexed xs, (i `mod` n) /= 0]
    where
        indexed xs = zip xs [1..(length xs)]
