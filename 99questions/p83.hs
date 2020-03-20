import Data.List

nodes :: Eq a => [(a,a)] -> [a]
nodes xs = nub $ map fst xs ++ map snd xs


subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
        where
            yss = subs xs

spanningTree xs = [a | a <- patterns, (length $ nodes a) == len]
    where
        len = length $ nodes xs
        patterns = [a | a <- subs xs, length a == (len-1)]



test_dat = [('a','b'),('a','d'),('b','c'),('b','e'),('c','e'),('d','e'),('d','f'),('d','g'),('e','h'),('f','g'),('g','h')]

test_dat2 = [('a','b'),('a','c'),('a','d'),('b','d'),('c','d')]


test_dat3 = [(1,2),(1,3),(1,4),(2,4),(2,3),(3,4)]


main = do 
        print $ length $ spanningTree test_dat
        print $ length $ spanningTree test_dat3
