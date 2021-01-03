
import Data.List
import Data.Ord


distributeGifts :: [(Int,Int)] -> Int
distributeGifts xs = sum $ mins ++ maxs 
    where
        ds = map snd $ sortBy (comparing fst) $ map (\(a,b) -> (b-a, (a,b))) xs
        l = length xs `div` 2
        maxs = map snd $ take l ds
        mins = map fst $ drop l ds



{-
distributeGifts :: [(Int,Int)] -> Int
distributeGifts xs = minimum $ map sum all
    where
        l = length xs `div` 2
        comb = permutations $ (replicate l fst) ++ (replicate l snd)
        patn xs ys = zipWith (\f d -> f d) ys xs
        all = map (patn xs) comb
        -}



        
data1 =  [(2,5),(3,4),(3,7),(1,5),(6,6),(7,100)]
data2 =  [(2,5),(3,6),(4,7),(5,100)]
