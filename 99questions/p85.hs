import Data.List

data GraphTerm a = Graph [a] [(a,a)] deriving (Show)



-- 全写像のパターンを求める
bijPatn :: (Eq a, Eq b) => [a] -> [b] -> [[(a,b)]]
bijPatn [x] [y] = [[(x,y)]]
bijPatn (x:xs) ys = concat [map (\l -> (x,y) : l) (bijPatn xs (delete y ys)) | y <-ys]

-- 写像先に変換する
findbi :: Eq a => a -> [(a,b)] -> b    
findbi x ys = head [b | (a,b) <- ys, x==a]

trans :: Eq a => GraphTerm a -> [(a,b)] -> GraphTerm b
trans (Graph ns es) bij = Graph nns nes
    where
        nns = [findbi a bij | a <- ns]
        nes = [(findbi a bij, findbi b bij) | (a,b) <- es]


iso (Graph nxs exs) (Graph nys eys) = bijPatn nxs nys

{--
iso (Graph nxs exs) (Graph nys eys) = [ trans exs bij | bij <- cmbs]
        where
          cmbs = bijPatn nxs nys  
          --}



graphG1 = Graph [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = Graph [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]

g1 = Graph [1,2,3] [(1,2),(1,3)]
g2 = Graph [4,5,6] [(4,5),(4,6)]
