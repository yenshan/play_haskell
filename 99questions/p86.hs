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

---             
--- a)
--
degree :: Ord a => GraphTerm a -> a -> Int
degree graph node = length $ getAdj node graph

---
--- b)
---
sortNodeByDegree :: Ord a => GraphTerm a -> [a]
sortNodeByDegree g@(Graph ns es) = map (fst) $ (sortBy.flip) (comparing snd) degs 
    where
       degs = [(n, degree g n) | n <- ns]

---
--- c)
---
kcolor [] _ _ res = res
kcolor n@(x:xs) c g res
    | elem c adjcolor = kcolor n (c+1) g res
    | otherwise = kcolor xs 1 g $ res++[(x,c)]
    where
       adjcolor = [c | (a,c) <- res, elem a $ getAdj x g]


kColor g = kcolor (sortNodeByDegree g) 1 g [] 

        
testG = Graph ['a','b','c','d','e','f','g','h','i','j'] [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]

main = do
        print $ kColor testG

