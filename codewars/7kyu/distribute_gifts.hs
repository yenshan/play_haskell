

distributeGifts :: [Int] -> Int
distributeGifts = sum . map maxFac


maxFac n = if facs == [] then n else n `div` (head facs)
    where
        facs = [a | a <- [2..n-1], n `mod` a == 0] 


data1 = [11,22,33]
