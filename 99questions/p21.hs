
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = []
insertAt x ys 1 = x : ys
insertAt x (y:ys) n = y : insertAt x ys (n-1)

