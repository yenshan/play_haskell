
scoreTable = [
    ("Wild", 10),
    ("Star", 9),
    ("Bell", 8),
    ("Shell", 7),
    ("Seven", 6),
    ("Cherry", 5),
    ("Bar", 4),
    ("King", 3),
    ("Queen", 2),
    ("Jack", 1)
    ]

wildPoint "Wild" = 2
wildPoint _ = 1

scoreTwoSame x = (snd.head) $ filter (\(s,_) -> s == x) scoreTable

scoreThreeSame x = 10 * scoreTwoSame x

fruitScore [a,b,c]
    | a == b && b == c = scoreThreeSame a
    | a == b = scoreTwoSame a * wildPoint c
    | a == c = scoreTwoSame a * wildPoint b
    | b == c = scoreTwoSame b * wildPoint a
    | otherwise = 0

fruit :: [[String]] -> [Int] -> Int
fruit reels spins = fruitScore $ [ reel !! spin | (reel, spin) <- zip reels spins]
        
main = do
        let ans = fruit [["Wild","Star","Bell","Shell","Seven","Cherry","Bar","King","Queen","Jack"],
               ["Bar", "Wild", "Queen", "Bell", "King", "Seven", "Cherry", "Jack", "Star", "Shell"],
               ["Bell", "King", "Wild", "Bar", "Seven", "Jack", "Shell", "Cherry", "Queen", "Star"]]
               [0,1,2]
        print ans
        print $ fruit [["King", "Cherry", "Bar", "Jack", "Seven", "Queen", "Star", "Shell", "Bell", "Wild"],
               ["Bell", "Seven", "Jack", "Queen", "Bar", "Star", "Shell", "Wild", "Cherry", "King"],
               ["Wild", "King", "Queen", "Seven", "Star", "Bar", "Shell", "Cherry", "Jack", "Bell"]]
              [9,8,7]

