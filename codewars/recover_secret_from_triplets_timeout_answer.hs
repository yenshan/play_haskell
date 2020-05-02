import Data.List


secret = "whatisup"
triplets = ["tup" ,"whi" ,"tsu" ,"ats" ,"hap" ,"tis" ,"whs" ]


permutation [] = [[]]
permutation xs = concat [ map (x:) $ permutation (delete x xs) | x <- xs]

match words triplet = match_words == triplet
    where
        match_words = filter (\x -> x `elem` triplet) words

recoverSecret triplets = head $ filter match_ $ permutation words
    where
        words = nub $ concat triplets
        match_ w = all (match w) triplets
        
