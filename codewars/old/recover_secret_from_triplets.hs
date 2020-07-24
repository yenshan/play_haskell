import Data.List

import Debug.Trace

secret = "whatisup"
test_triplets = ["tup" ,"whi" ,"tsu" ,"ats" ,"hap" ,"tis" ,"whs" ]

isMin triplets x = not $ any (\ws -> x `elem` tail ws) triplets

recoverSecret :: [[Char]] -> [Char]
recoverSecret [] = []
recoverSecret triplets = minc : recoverSecret new_triplets
    where
        words = nub $ concat triplets
        trp = filter (\x -> length x > 1) triplets
        minc = head $ filter (isMin trp) words
        new_triplets = filter (not.null) $ map (delete minc) triplets

