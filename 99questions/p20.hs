

removeAt :: Int -> [a] -> (a,[a])
removeAt n xs = (get n xs, remove n xs)
    where
        get n xs = head [a | (a,i) <- indexed xs, i == n]
        remove n xs = [a | (a,i) <- indexed xs, i /= n]
        indexed xs = zip xs [1..(length xs)]

