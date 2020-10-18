
duplicateSandwich :: (Eq a) => [a] -> [a]
duplicateSandwich xs = takeWhile (/=e) $ tail $ dropWhile (/=e) xs
    where
        dupElem (x:xs) = if x `elem` xs then x else dupElem xs
        e = dupElem xs



testdat = [0, 1, 2, 3, 4, 5, 6, 1, 7, 8]
