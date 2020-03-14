import Data.List
import Data.Ord


fst' (a,_,_) = a
snd' (_,a,_) = a


nodes :: (Eq a,Num b) => [(a,a,b)] -> [a]
nodes xs = nub $ map fst' xs ++ map snd' xs


subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
        where
            yss = subs xs

combs :: (Eq a,Num b) => [a] -> [(a,a,b)] -> [[(a,a,b)]]
combs xs ys = [a | a <- patterns, (length $ nodes a) == len]
    where
        len = length xs
        patterns = [a | a <- subs ys, length a == (len-1)]


prim xs ys = fst $ head $ sortBy (comparing snd) tsl
    where
      treeSize xs = sum [l | (_,_,l) <- xs]
      tsl = [(t, treeSize t) | t <- combs xs ys]


main = do  
         print $ prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
