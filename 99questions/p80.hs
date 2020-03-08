import Data.List

data GraphTerm a = Graph [a] [(a,a)] deriving (Show)
data AdjTerm a = Adj [(a,[a])] deriving (Show)


test_g_dat = Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]

test_a_dat = Adj [('b',"cf"),('c',"bf"),('d',""),('f',"bck"),('g',"h"),('h',"g"),('k',"f")]


graphToAdj :: Ord a => GraphTerm a -> AdjTerm a
graphToAdj (Graph xs ys) = Adj [(x, adjs x) | x <- xs]
        where
            adjs x = sort $ [b | (a, b) <- ys, a == x] ++ [a | (a, b) <- ys, b == x]


equal :: Ord a => (a,a) -> (a,a) -> Bool
equal (a,b) (c,d) = sort [a,b] == sort [c,d]

adjToGraph :: Ord a => AdjTerm a -> GraphTerm a
adjToGraph (Adj xs) = Graph nodes edges
        where
            nodes = [a | (a,_) <- xs]
            find x = [b | (a,b) <- xs, a == x]
            edge x = [(x, b) | ys <- find x, b <- ys] 
            edges = nubBy equal $ concat [edge x | x <- nodes]

main = do
        print $ graphToAdj test_g_dat
        print $ adjToGraph test_a_dat
