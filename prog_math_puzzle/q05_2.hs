
change_coin :: Int -> [Int] -> Int -> Int
change_coin 0 _ _ = 1
change_coin _ _ 0 = 0
change_coin _ [] _ = 0
change_coin money (x:xs) max_coins
    = sum [ change_coin (money - x * use) xs (max_coins - use)
            | use <- [0..max_coins]]

main :: IO ()
main = do print $ change_coin 1000 [10,50,100,500] 15
