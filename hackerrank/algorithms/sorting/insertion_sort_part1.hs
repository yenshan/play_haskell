--
-- https://www.hackerrank.com/challenges/insertionsort1/problem
--
insert_once :: [Int] -> Int -> [Int] -> ([Int], [Int], [Int])
insert_once rs v [] = (rs ++ [v], [], [])    
insert_once rs v (x:xs)
    | v < x = (rs ++ [x], [v], xs)
    | otherwise = (rs ++ v:x:xs, [], [])

intToString :: [Int] -> [Char]
intToString [] = []
intToString (x:xs) = show x ++ " " ++ intToString xs
    
insert_s :: [Int] -> Int -> [Int] -> IO ()
insert_s rs v hs = do
                    let (l,s,r) = insert_once rs v hs
                    if s == [] then
                        putStrLn $ intToString . reverse $ l
                    else do
                        putStrLn $ intToString . reverse $ (l ++ (last l) : r)
                        insert_s l (head s) r

main :: IO ()
main = do
    _ <- getLine
    arr_tmp <- getLine
    let array_s = words arr_tmp
    let array = map (read :: String -> Int) array_s
    let r_array = reverse array
    insert_s [] (head r_array) (tail r_array)


