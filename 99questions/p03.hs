

_elementAt :: Int -> [a] -> Int -> a
_elementAt n (x:xs) k
    | n == k = x
    | otherwise = _elementAt (n+1) xs k

elementAt :: [a] -> Int -> a
elementAt xs k = _elementAt 1 xs k
