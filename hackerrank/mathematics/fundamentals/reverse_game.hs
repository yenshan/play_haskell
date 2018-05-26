;;
;; https://www.hackerrank.com/domains/mathematics/fundamentals/2
;;

reverse_game :: (Int,Int) -> [Int]
reverse_game (x,y) 
    | x == y = [x]
    | otherwise = [x,y] ++ reverse_game (x-1, y+1)

find_number :: Int -> [Int] -> Int -> Int
find_number n (x:xs) i
    | n == x = i
    | otherwise = find_number n xs (i+1)


do_loop :: Int -> IO ()
do_loop 0 = return ()                                              
do_loop nl = do
              nk_tmp <- getLine
              let nk = words nk_tmp
              let n = read $ nk!!0 :: Int
              let k = read $ nk!!1 :: Int
              let rev_res = reverse_game (n-1,0)
              print $ find_number k rev_res 0
              do_loop (nl-1)

main :: IO ()
main = do
         t_tmp <- getLine
         let t = read $ t_tmp :: Int
         do_loop t

