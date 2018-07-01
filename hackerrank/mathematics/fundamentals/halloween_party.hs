
do_loop 0 = return ()
do_loop t = do
              k_tmp <- getLine
              let k = read $ k_tmp :: Int
              print $ maximum [ a * (k - a) | a <- [1..(k - 1)]]
              do_loop (t - 1)

main = do
        t_tmp <- getLine
        let t = read $ t_tmp :: Int
        do_loop t
