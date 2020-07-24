import Data.List


sumDigit :: Integer -> Integer
sumDigit n = sum $ map (\x -> read x ::Integer) $ map (:"") $ show n


isRoot :: Integer -> Integer -> Bool
isRoot 1 _ = True
isRoot _ 1 = False
isRoot n x
    | n `mod` x == 0 = isRoot (n `div` x) x
    | otherwise = False


candidates = nub $ filter (>10) $ [ n^e | n <- [2..200], e <- [2..20]]


powerSumDigTerm :: Int -> Integer
powerSumDigTerm n = digiSeq !! (n-1)
    where
        digiSeq = sort $ filter (\x -> isRoot x (sumDigit x)) $ candidates

{--
powerSumDigTerm :: Int -> Integer
powerSumDigTerm n = digiSeq !! (n-1)
    where
        digiSeq = filter (\x -> isRoot x (sumDigit x)) $ iterate succ 81
        --}



