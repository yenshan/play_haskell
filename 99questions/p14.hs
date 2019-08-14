
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs


dupli' :: [a] -> [a]
dupli' xs = dupli_ xs []
    where
        dupli_ [] res = res
        dupli_ (x:xs) res = dupli_ xs (res++[x,x])

