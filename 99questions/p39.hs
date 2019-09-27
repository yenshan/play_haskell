

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = length (divisable n) == 0
            where
                divisable n = [a | a <- [2..(n `div` 2)], n `mod` a == 0]

primesR :: Int -> Int -> [Int]
primesR l u = [a | a <- [l+1 .. u-1], isPrime a]
