
is_sequential :: [Int] -> Bool
is_sequential (x:xs)
    | null xs = True
    | x /= head xs && x + 1 /= head xs = False
    | otherwise = is_sequential xs

is_rainbow_array :: [Int] -> Bool
is_rainbow_array xs = 
    left_half == reverse right_half 
    && is_sequential left_half_with_center
    where
        hl = length xs `quot` 2
        c = length xs `mod` 2
        left_half = take hl xs
        right_half = drop (hl + 1) xs
        left_half_with_center = take (hl+c) xs

answer_str :: Bool -> String      
answer_str True = "yes"
answer_str False = "no"


do_loop :: Int -> IO ()
do_loop 0 = return ()
do_loop t = do
             n_temp <- getLine
             a_temp <- getLine
             let as = map read $ words a_temp :: [Int]
             putStrLn $ answer_str (is_rainbow_array as)
             do_loop (t - 1)

main :: IO ()
main = do
        t_temp <- getLine
        let t = read $ t_temp :: Int
        do_loop t
