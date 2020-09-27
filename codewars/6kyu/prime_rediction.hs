--
-- https://www.codewars.com/kata/59aa6567485a4d03ff0000ca
--
module PrimeReduction where

solve :: Int -> Int -> Int
solve a b = length $ filter isSumSqrToOne primes
    where
        primes = filter isPrime [a..b-1]


isSumSqrToOne n | ssqr >= 10 = isSumSqrToOne ssqr
                | otherwise = if ssqr `elem` [1,7] then True else False
        where
            ssqr = sum $ map (^2) $ toDigitList n

toDigitList 0 = []
toDigitList n = toDigitList (n `div` 10) ++ [n `mod` 10]


isPrime 1 = False
isPrime n = divisor == [n]
    where
        divisor = [a | a <- [2..n], n `mod` a == 0]



sumSquareList n = [ss] ++ sumSquareList ss
    where
        ss = sum $ map (^2) $ toDigitList n

