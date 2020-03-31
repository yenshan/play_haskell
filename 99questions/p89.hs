import Data.List
import Data.Ord

data GraphTerm a = Graph [a] [(a,a)] deriving (Show)
data AdjTerm a = Adj [(a,[a])] deriving (Show)


graphToAdj :: Ord a => GraphTerm a -> AdjTerm a
graphToAdj (Graph xs ys) = Adj [(x, adjs x) | x <- xs]
        where
            adjs x = sort $ [b | (a, b) <- ys, a == x] ++ [a | (a, b) <- ys, b == x]


getdat n [] = 0
getdat n (x:xs) = let (y,b) = x in if y==n then b else getdat n xs

getadj n [] = []
getadj n (x:xs) = let (y,b) = x in if y==n then b else getadj n xs


getsides g@(Adj xs) n sl = [(n,s)] ++ adjl ++ sl
        where
            oppsit 0 = 1
            oppsit 1 = 0
            s = getdat n sl
            adjl = [(a, oppsit s) | a <- getadj n xs]       
        

bipartitle g@(Graph xs _) = length sl == length xs
    where
        adj = graphToAdj g
        bipartitle_ [] sl = sl
        bipartitle_ (n:ns) sl = bipartitle_ ns $ getsides adj n sl
        sl = nub $ bipartitle_ xs []


testG = Graph [1,2,3,4,5] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)]

testG2 = Graph [1,2,3,4,5] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)]


main = do
         print $ bipartitle testG
         print $ bipartitle testG2


