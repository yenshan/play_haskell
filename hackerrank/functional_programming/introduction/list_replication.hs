rep :: Int -> a -> [a]
rep 0 x = []
rep n x = [x] ++ rep (n - 1) x 

list_rep :: Int -> [Int] -> [Int]
list_rep n [] = []
list_rep n (x:xs) = rep n x ++ list_rep n xs

list_rep2 :: Int -> [a] -> [a]
list_rep2 n xs = concat (map (\x -> take n (repeat x)) xs)

list_rep3 :: Int -> [a] -> [a]
list_rep3 n xs = concat [ take n (repeat a) | a <- xs ]

