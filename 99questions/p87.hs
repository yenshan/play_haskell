import Data.List
import Data.Ord

data GraphTerm a = Graph [a] [(a,a)] deriving (Show)
data AdjTerm a = Adj [(a,[a])] deriving (Show)


graphToAdj :: Ord a => GraphTerm a -> AdjTerm a
graphToAdj (Graph xs ys) = Adj [(x, adjs x) | x <- xs]
        where
            adjs x = sort $ [b | (a, b) <- ys, a == x] ++ [a | (a, b) <- ys, b == x]


getAdj x g = head [b | (a,b) <- getadj $ graphToAdj g, a==x]
    where
       getadj (Adj xs) = xs 



depthfirst_ _ s [] path = path++[s]
depthfirst_ g s (x:xs) path
    | elem x path = depthfirst_ g s xs path
    | otherwise = depthfirst_ g x (getAdj x g) (path++[s])


depthFirst g s = depthfirst_ g s (getAdj s g) []


testG = Graph [1,2,3,4,5,6,7] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]



main = do
         print $ depthFirst testG 1
