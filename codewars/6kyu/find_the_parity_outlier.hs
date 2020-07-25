--
-- Find The Parity Oullier
--
module Kata (findOutlier) where

findOutlier :: [Int] -> Int
findOutlier xs = 
    if cntOdd xs == 1 then
         head $ dropWhile even xs
    else 
         head $ dropWhile odd xs
    where
        cntOdd = length . filter odd
