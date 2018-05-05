

do_loop :: Int -> IO ()
do_loop 0 = return ()
do_loop t = do
              n_temp <- getLine
              let n = read $ n_temp :: Integer
              print $ (n^2) `mod` 1000000007
              do_loop (t - 1)

main :: IO ()
main = do
        t_temp <- getLine
        let t = read $ t_temp :: Int
        do_loop t
