module Feynman where

countSquares :: Integer -> Integer
countSquares n = sum $ map numSquares [1..n]
    where
        numSquares :: Integer -> Integer
        numSquares m = sum [ 1 | x <- [0..n-m], y <- [0..n-m]] 



