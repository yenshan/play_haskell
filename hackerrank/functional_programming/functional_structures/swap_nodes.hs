--
-- https://www.hackerrank.com/challenges/swap-nodes/problem
--
data Tree = Nil | Node Int Tree Tree deriving Show

new_node :: Int -> Tree
new_node n | n == (-1) = Nil
           | otherwise = Node n Nil Nil

append_node :: Tree -> (Int,Int,Int) -> Tree
append_node Nil _ = Nil
append_node (Node n l r) (i,nl,nr)
    | n == i = Node i (new_node nl) (new_node nr)
    | otherwise = Node n 
                       (append_node l (i,nl,nr))
                       (append_node r (i,nl,nr))

swap_node_iter :: Tree -> Int -> Int -> Int -> Tree
swap_node_iter Nil _ _ _ = Nil
swap_node_iter (Node n l r) depth k kn
    | depth == kn = Node n 
                         (swap_node_iter r (depth + 1) k (kn + k))
                         (swap_node_iter l (depth + 1) k (kn + k))
    | otherwise = Node n 
                       (swap_node_iter l (depth + 1) k kn)
                       (swap_node_iter r (depth + 1) k kn)

swap_node :: Tree -> Int -> Tree
swap_node x k = swap_node_iter x 1 k k

trans_data :: [[Int]] -> [(Int,Int,Int)] 
trans_data xs = map (\(i, (l,r)) -> (i,l,r)) ixs
                where
                    ixs = (zip [1..] (map (\a -> (a !! 0, a !! 1)) xs))

print_tree :: Tree -> IO ()
print_tree Nil = return () 
print_tree (Node n l r)
    = do
        print_tree l
        putStr $ show n ++ " "
        print_tree r

doseq :: Tree -> Int -> IO ()
doseq _ 0 = return ()
doseq tree n = do
                k_temp <- getLine
                let k = read $ k_temp :: Int
                let ntree = swap_node tree k
                print_tree ntree
                putChar '\n'
                doseq ntree (n - 1)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getMultipleLines n
    let a = map ( map ( read :: String -> Int ) . words ) a_temp
    let tree = foldl append_node (new_node 1) (trans_data a)
    t_temp <- getLine
    let t = read t_temp :: Int
    doseq tree t 

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret    
