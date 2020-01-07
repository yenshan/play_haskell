
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)


atLevel Empty _ = []    
atLevel (Branch a _ _) 1 = [a]
atLevel (Branch _ l r) n = atLevel l (n-1) ++ atLevel r (n-1)


completeBinaryTree 0 = Empty    
completeBinaryTree n 
    | odd n = (Branch 'x' (completeBinaryTree divn) (completeBinaryTree divn))
    | otherwise = (Branch 'x' (completeBinaryTree (divn1+1)) (completeBinaryTree divn1))
    where
      divn = n `div` 2
      divn1 = (n-1) `div` 2

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

levelNodes :: Tree a -> [Int]
levelNodes t = takeWhile (/=0) $ map (\n -> length $ atLevel t n) [1..]

isCompleteBinaryTree t = sum comp_result == len
    where
        level_nodes = map fromIntegral (levelNodes t)
        len = (length level_nodes)-1
        nodes_list = [2 ** (i-1) | i <- [1..fromIntegral len]]
        comp_result = map (\(a, b) -> if a==b then 1 else 0) $ zip level_nodes nodes_list

main = do
         print $ completeBinaryTree 4
         print $ isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
         return ()

         
