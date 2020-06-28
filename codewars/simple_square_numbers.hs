-- https://www.codewars.com/kata/5edc8c53d7cede0032eb6029


import Data.List
import Data.Ord



divisors n = 1 : if even n then evenDivs else oddDivs
    where
        max = round $ sqrt (fromIntegral n)
        oddDivs = [a | a <- [3,5..max], n `mod` a == 0]
        evenDivs = [a | a <- [2,4..max], n `mod` a == 0]

solve :: Integer -> Maybe Integer
solve n = if null cads then Nothing else ans (head cads)
    where
        cads = reverse $ filter (\(x,y) -> x < y && (y - x) `mod` 2 == 0) $ map (\x -> (x, n `div` x)) $ divisors n
        ans (x,y) = Just (((y-x) `div` 2)^2)



