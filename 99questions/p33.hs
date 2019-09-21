


myGCDp :: Int -> Int -> Int
myGCDp x y | x == y = x
          | x < y = myGCDp x (y - x)
          | otherwise = myGCDp (x - y) y
    
myGCD :: Int -> Int -> Int
myGCD x y = myGCDp (abs x) (abs y)

coprime :: Int -> Int -> Bool
coprime x y = myGCD x y == 1

