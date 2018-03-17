
main :: IO ()
main = do
    n_tmp <- getLine
    let n = read n_tmp :: Int
    sequence_ [putStrLn $ "Hello World" | _ <- [1..n]]


