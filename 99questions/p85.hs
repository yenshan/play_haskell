import Data.List

data GraphTerm a = Graph [a] [(a,a)] deriving (Show,Eq)
data AdjTerm a = Adj [(a,[a])] deriving (Show)

adj x xs = head [b | (a,b) <- xs, a==x]

instance Eq a => Eq (AdjTerm a) where
    (Adj xs) == (Adj ys) = ne == []
        where
           ne = [a | (a,b) <- xs, b /= adj a ys]


graphToAdj :: Ord a => GraphTerm a -> AdjTerm a
graphToAdj (Graph xs ys) = Adj [(x, adjs x) | x <- xs]
        where
            adjs x = sort $ [b | (a, b) <- ys, a == x] ++ [a | (a, b) <- ys, b == x]



-- 全写像のパターンを求める
bijPatn :: (Eq a, Eq b) => [a] -> [b] -> [[(a,b)]]
bijPatn [x] [y] = [[(x,y)]]
bijPatn (x:xs) ys = concat [map (\l -> (x,y) : l) (bijPatn xs (delete y ys)) | y <-ys]

-- 写像先に変換する
findbi :: Eq a => a -> [(a,b)] -> b    
findbi x ys = head [b | (a,b) <- ys, x==a]

-- Graphを写像先に変換する
trans :: Eq a => GraphTerm a -> [(a,b)] -> GraphTerm b
trans (Graph ns es) bij = Graph nns nes
    where
        nns = [findbi a bij | a <- ns]
        nes = [(findbi a bij, findbi b bij) | (a,b) <- es]

-- iso
-- すべての写像のパターンを求めて、
-- 写像に変換したあと、同じGraphになるかどうかをチェックする
iso :: Ord a => GraphTerm a -> GraphTerm a -> Bool
iso gx@(Graph nxs exs) gy@(Graph nys eys) = sames /= []
        where
            allbij = bijPatn nxs nys
            sames = [tgx | bij <- allbij, let tgx = trans gx bij, graphToAdj tgx == graphToAdj gy]


graphG1 = Graph [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = Graph [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]

g1 = Graph [1,2,3] [(1,2),(1,3)]
g2 = Graph [4,5,6] [(4,5),(4,6)]


main = do
        print $ iso graphG1 graphH1

