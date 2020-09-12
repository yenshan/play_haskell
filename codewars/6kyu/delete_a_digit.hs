

toList d | d < 10 = [d]
         | otherwise = toList (d `div` 10) ++ [(d `mod` 10)]


toDigit xs = sum $ zipWith (*) (reverse xs) (iterate (*10) 1)


delete i xs = take i xs ++ drop (i+1) xs   


deleteDigit :: Int -> Int
deleteDigit d = maximum $ map toDigit pattern
    where
        dlist = toList d
        pattern = [ delete i dlist | i <- [0..length dlist-1]]

