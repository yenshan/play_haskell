import Data.List
import Data.Ord

data BTree = Leaf String Int | Node String Int BTree BTree deriving Show


combine :: BTree -> BTree -> BTree
combine x@(Leaf a b) y@(Leaf c d) = Node (a++c) (b+d) x y 
combine x@(Leaf a b) y@(Node c d _ _) = Node (a++c) (b+d) x y
combine x@(Node a b _ _) y@(Leaf c d) = Node (a++c) (b+d) x y
combine x@(Node a b _ _) y@(Node c d _ _) = Node (a++c) (b+d) x y

myCompare :: BTree -> BTree -> Ordering
myCompare (Leaf _ b) (Leaf _ d) = compare b d
myCompare (Leaf _ b) (Node _ d _ _) = compare b d
myCompare (Node _ b _ _) (Leaf _ d) = compare b d
myCompare (Node _ b _ _) (Node _ d _ _) = compare b d


makeBTree :: [BTree] -> BTree
makeBTree [x] = x
makeBTree (x:y:xs) = makeBTree $ sortBy myCompare (combine x y : xs)

convToBTree :: (Char,Int) -> BTree
convToBTree (a,b) = Leaf [a] b

huffmanTree :: [(Char,Int)] -> BTree
huffmanTree xs = makeBTree $ sortBy myCompare $ map convToBTree xs

encode :: String -> BTree -> [(Char,String)]
encode code (Leaf a b) = [(head a,code)]
encode code (Node x y l r) = encode (code++"0") l ++ encode (code++"1") r

encodeHuffman :: [(Char,Int)] -> [(Char,String)]
encodeHuffman xs = sortBy (comparing fst) $ encode "" $ huffmanTree xs

main = do 
        print $ encodeHuffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
        return ()

