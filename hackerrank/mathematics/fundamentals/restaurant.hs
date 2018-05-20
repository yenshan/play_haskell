--
-- https://www.hackerrank.com/challenges/restaurant/problem
--


mgcd :: Int -> Int -> Int
mgcd l 0 = l
mgcd l b | l < b = mgcd l (b `mod` l)
         | l > b = mgcd b (l `mod` b)
         | otherwise = l

do_loop :: Int -> IO ()
do_loop 0 = return ()
do_loop n = do
              val_tmp <- getLine
              let lb = words val_tmp
              let l = read $ lb!!0 :: Int
              let b = read $ lb!!1 :: Int
              let sl = mgcd l b
              print $ (l * b) `div` (sl * sl)
              do_loop (n-1)
    
main :: IO ()
main = do
        t_tmp <- getLine
        let t = read $ t_tmp :: Int
        do_loop t
