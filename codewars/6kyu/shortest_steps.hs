

steps :: Int -> Int
steps 1 = 0
steps 2 = 1
steps n | n `mod` 2 == 0 = 1 + steps (n `div` 2) 
        | otherwise = 1 + steps (n - 1)


