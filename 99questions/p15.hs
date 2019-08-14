

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = repeat x n ++ repli xs n
    where
        repeat x n = [x | _ <- [1..n]]
