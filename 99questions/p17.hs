

mytake :: [a] -> Int -> [a]
mytake _ 0 = []
mytake (x:xs) n = x : mytake xs (n-1)

mydrop :: [a] -> Int -> [a]
mydrop xs 0 = xs
mydrop (x:xs) n = mydrop xs (n-1)

split :: [a] -> Int -> [[a]]
split xs n = [mytake xs n] ++ [mydrop xs n]
