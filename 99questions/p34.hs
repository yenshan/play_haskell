

myGCDp :: Int -> Int -> Int
myGCDp x y | x == y = x
          | x < y = myGCDp x (y - x)
          | otherwise = myGCDp (x - y) y
    
myGCD :: Int -> Int -> Int
myGCD x y = myGCDp (abs x) (abs y)

coprime :: Int -> Int -> Bool
coprime x y = myGCD x y == 1


totient :: Int -> Int
totient 1 = 1
totient m = length [a | a <- [1..(m-1)], coprime a m]
