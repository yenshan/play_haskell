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

_path :: Ord a => a -> a -> AdjTerm a -> [a] -> [[a]]
_path s d ys ps = if s == d then
                     [ps]
                  else
                     concat [_path a d ys (ps ++ [a]) | a <- nextp]
    where
        nextp = filter (\x -> not (elem x ps)) $ _find s ys

path :: Ord a => a -> a -> [(a,a)] -> [[a]]
path s d xs = _path s d adj [s] 
    where
        adj = arcToAdj $ Arc (node xs) xs


----
----

test_dat = [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]

main = do
         print $ arcToAdj $ Arc (node test_dat) test_dat
         print $ path 1 4 test_dat
         print $ path 2 6 test_dat

