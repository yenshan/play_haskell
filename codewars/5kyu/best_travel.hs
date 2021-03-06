

chooseBestSum :: Int -> Int -> [Int] -> Maybe Int
chooseBestSum t k ls = if dists == [] then Nothing else Just (maximum dists)
    where
        towns = filter (\x -> length x == k) $ subset ls
        dists = filter (\x -> x <= t) $ map sum towns


subset [] = [[]]
subset (x:xs) = subset xs ++ map (x:) (subset xs)

subs 0 _ = [[]]
subs _ [] = [[]]
subs n (x:xs) = subs n xs ++ map (x:) (subs (n-1) xs)

data1 :: [Int]
data1 = [50, 55, 56, 57, 58]

