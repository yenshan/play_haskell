import Data.List


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

count_num :: (Num t, Eq a) => t -> [a] -> [(a,t)]
count_num n [x] = [(x,(n+1))]
count_num n (x:y:xs) | x /= y = (x, (n+1)) : count_num 0 (y:xs)
                     | otherwise = count_num (n+1) (y:xs)

prime_factors_mult :: Num t => Int -> [(Int, t)]
prime_factors_mult n = count_num 0 $ sort $ primeFactor n


phi m = foldl fn 1 (prime_factors_mult m)
        where
            fn res (p,m) = let p1 = fromIntegral p
                               m1 = fromIntegral m
                           in res * (p1 - 1) * p1 ** (m1 - 1)


