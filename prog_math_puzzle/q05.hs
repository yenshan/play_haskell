

change_coin :: Int -> Int -> Int
change_coin money max_coins = 
    sum [1 | m10 <- [0..max_coins],
             m50 <- [0..max_coins],
             m100 <- [0..max_coins],
             m500 <- [0..max_coins],
             m10 + m50 + m100 + m500 <= max_coins,
             money == (10*m10 + 50*m50 + 100*m100 + 500*m500)]

 
