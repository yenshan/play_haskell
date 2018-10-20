--
--https://www.hackerrank.com/challenges/sherlock-and-the-beast/problem
--
import Control.Monad

findDecent n res = 
    let nn = n - 5
    in if nn < 0 then (-1,-1)
       else 
        if nn `mod` 3 == 0 then (nn `quot` 3, res+1)
        else findDecent nn (res+1)

decentNumber n
   | 0 == (n `mod` 3) = take (3 * (n `quot` 3)) (repeat '5')
   | otherwise = if ns == "" then "-1" else ns
                 where
                     (n_of_five,n_of_three) = findDecent n 0
                     fc = take (n_of_five*3) (repeat '5')
                     tc = take (n_of_three*5) (repeat '3')
                     ns = fc ++ tc

main :: IO ()
main = do
    t_tmp <- getLine
    let t = read t_tmp :: Int
    forM_ [1..t] $ \a0 -> do
        n_temp <- getLine
        let n = read n_temp :: Int
        putStrLn $ decentNumber n

