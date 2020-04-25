import Data.List
import Data.Ord

data GraphTerm a = Graph [a] [(a,a)] deriving (Show)
data AdjTerm a = Adj [(a,[a])] deriving (Show)


graphToAdj :: Ord a => GraphTerm a -> AdjTerm a
graphToAdj (Graph xs ys) = Adj [(x, adjs x) | x <- xs]
        where
            adjs x = sort $ [b | (a, b) <- ys, a == x] ++ [a | (a, b) <- ys, b == x]


-- find value of key 'n' in list of (key,value), return 0 when not found.
findDat :: (Num b, Eq a) => a -> [(a,b)] -> b
findDat n [] = 0
findDat n ((x,v):xs) | x==n = v 
                     | otherwise = findDat n xs

findAdj :: Eq a => a -> [(a,[a])] -> [a]
findAdj n [] = []
findAdj n ((x,v):xs) | x==n = v
                     | otherwise = findAdj n xs

setNoToAdjs :: (Eq a, Eq b, Num b) => [(a,b)] -> a -> AdjTerm a -> [(a,b)]
setNoToAdjs node_no_list me (Adj xs) = [(me, myNo)] ++ adjl ++ node_no_list
        where
            oppsit 0 = 1
            oppsit 1 = 0
            myNo = findDat me node_no_list
            adjl = [(a, oppsit myNo) | a <- findAdj me xs]       
        

bipartitle g@(Graph xs _) = length sl == length xs
    where
        setNoToNode r x = setNoToAdjs r x (graphToAdj g)
        sl = nub $ foldl setNoToNode [] xs


testG = Graph [1,2,3,4,5] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)]

testG2 = Graph [1,2,3,4,5] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)]


main = do
         print $ bipartitle testG
         print $ bipartitle testG2


