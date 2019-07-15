

elementAtN n (x:xs) k
    | n == k = x
    | otherwise = elementAtN (n+1) xs k

elementAt xs k = elementAtN 1 xs k
