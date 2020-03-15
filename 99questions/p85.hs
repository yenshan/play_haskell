import Data.List

data GraphTerm a = Graph [a] [(a,a)] deriving (Show)
data AdjTerm a = Adj [(a,[a])] deriving (Show)


adjs :: Eq a => a -> AdjTerm a -> [a]
adjs x (Adj ys) = head [as | (a,as) <- ys, a == x]

graphToAdj :: Ord a => GraphTerm a -> AdjTerm a
graphToAdj (Graph xs ys) = Adj [(x, adjs x) | x <- xs]
        where
            adjs x = sort $ [b | (a, b) <- ys, a == x] ++ [a | (a, b) <- ys, b == x]

{-
iso g1@(Graph x [xs]) g2@(Graph y [ys]) = [  | (a,b) <- combs, 
        where
            adj1 = graphToAdj g1
            adj2 = graphToAdj g2
            combs = [(x,y) | x <- xs, y <-ys]
            -}

-- 全写像のパターンを求める
combs (x:xs) ys res = map (\a -> res ++ a) [(x,y) : combs xs  | y <- ys]





graphG1 = Graph [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = Graph [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]


main = do
         print $ graphToAdj graphG1
         print $ graphToAdj graphH1
