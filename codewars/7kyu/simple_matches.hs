module SimpleMatches where 


solve :: String -> String -> Bool
solve [] [] = True
solve "*" _ = True
solve [] _ = False
solve _ [] = False
solve ('*':xs) (y:ys) | length xs == length ys = solve xs ys
                      | length xs > length ys = solve xs (y:ys)
                      | otherwise = solve ('*':xs) ys
solve (x:xs) (y:ys) | x == y = solve xs ys
                    | otherwise = False


main = do print $ solve "code*s" "codewars"

