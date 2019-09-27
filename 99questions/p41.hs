
isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = length (divisable n) == 0
            where
                divisable n = [a | a <- [2..(n `div` 2)], n `mod` a == 0]

primes :: Int -> Int -> [Int]
primes l u = [a | a <- [l..u], isPrime a]


combinations :: Int -> [a] -> [[a]]
combinations 1 xs = [[a] | a <- xs]
combinations n xs | n == length xs = [xs]
combinations _ [] = [[]]
combinations n (x:xs) = [x : a | a <- combinations (n-1) xs] ++ (combinations n xs)


goldbach :: Int -> [[Int]]
goldbach n = [a | a <- combinations 2 (primes 1 n), sum a == n]


goldbachList :: Int -> Int -> [[Int]]
goldbachList l u = [head b |  b <- gl, b /= []]
                   where
                     gl = [goldbach a | a <- [l..u], a `mod` 2 == 0]


goldbachList' :: Int -> Int -> Int -> [[Int]]
goldbachList' l u n = [a | a <- goldbachList l u, a!!0 > 50, a!!1 > 50]
