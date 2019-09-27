
isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = length (divisable n) == 0
            where
                divisable n = [a | a <- [2..(n `div` 2)], n `mod` a == 0]

primesR :: Int -> Int -> [Int]
primesR l u = [a | a <- [l+1 .. u-1], isPrime a]


combinations :: Int -> [a] -> [[a]]
combinations 1 xs = [[a] | a <- xs]
combinations n xs | n == length xs = [xs]
combinations n (x:xs) = [x : a | a <- combinations (n-1) xs] ++ (combinations n xs)


goldbach n = [a | a <- combinations 2 (primesR 2 n), sum a == n]
