

isPrime :: Int -> Bool
isPrime n = length (divisable n) == 0
            where
                divisable x = [a | a <- [2..x-1], x `mod` a == 0]
