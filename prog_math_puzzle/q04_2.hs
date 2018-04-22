

cut_bar :: Int -> Int -> Int
cut_bar len m = cut 1
                where
                  cut n | n >= len = 0
                        | n < m = 1 + cut (n + n)
                        | otherwise = 1 + cut (n + m)

main :: IO ()
main = do print $ cut_bar 20 3
          print $ cut_bar 100 5

