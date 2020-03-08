import Data.List

data ArcTerm a = Arc [a] [(a,a)] deriving (Show)
data AdjTerm a = Adj [(a,[a])] deriving (Show)

arcToAdj :: Ord a => ArcTerm a -> AdjTerm a
arcToAdj (Arc xs ys) = Adj [(x, adjs x) | x <- xs]
        where
            adjs x = sort $ [b | (a, b) <- ys, a == x]


node :: Eq a => [(a,a)] -> [a]
node xs = nub $ map fst xs ++ map snd xs

_find :: Eq a => a -> AdjTerm a -> [a]
_find x (Adj ys) = concat [b | (a,b) <- ys, a == x]

_cycle :: Ord a => a -> a -> AdjTerm a -> [a] -> [[a]]
_cycle s n ys ps 
    | s == n = [ps++[n]]
    | elem n ps = [] 
    | otherwise = concat [_cycle s a ys (ps++[n]) | a <- _find n ys]

gcycle :: Ord a => a -> [(a,a)] -> [[a]]
gcycle s xs = concat [_cycle s a adj [s] | a <- _find s adj] 
    where
        adj = arcToAdj $ Arc (node xs) xs


----
----

test_dat = [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]

main = do
         print $ arcToAdj $ Arc (node test_dat) test_dat
         print $ gcycle 2 test_dat
         print $ gcycle 1 test_dat

