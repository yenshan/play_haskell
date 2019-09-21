

isPrime :: Int -> Bool
isPrime n = length (divisable n) == 0
            where
                divisable x = [a | a <- [2..x-1], x `mod` a == 0]


primeFactorList n = [a | a <- [2..n-1], isPrime a, n `mod` a == 0]



primeFactor n = if pfl == [] then [n]
                else p1 :  primeFactor (n `div` p1)
                where
                   pfl = primeFactorList n    
                   p1 = head pfl



