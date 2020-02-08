
data Tree a = Node a [Tree a] deriving (Eq,Show)

tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [
                    Node 'f' [Node 'g' []],
                    Node 'c' [],
                    Node 'b' [Node 'd' [], Node 'e' []]
                 ]


display_lisp :: Tree Char -> String
display_lisp (Node a []) = [a]
display_lisp (Node a xs) = ['(',a] ++ concat [" " ++ display_lisp x | x <- xs] ++ [')']

anyChar :: [Char] -> (Char, [Char])
anyChar [] = (' ',"")
anyChar [x] = (x,"")
anyChar (x:' ':xs) = (x,xs)
anyChar (' ':x:xs) = (x,xs)
anyChar (x:xs) = (x,xs)

parse :: String -> ([Tree Char],String)
parse [] = ([],"")   
parse str = 
    let (c1,str1) = anyChar str
        (c2,str2) = anyChar str1
    in
    if c1==')' then
        ([],str1)
    else
      if c1=='(' then
        let (t,str3) = parse str2
        in ([Node c2 t],str3)
      else
        let (t,str3) = parse str1
        in 
           if str3=="" then
             ([Node c1 []] ++ t, str3)
           else
             let (t2,str4) = parse str3
             in ([Node c1 []] ++ t ++ t2, str4)
             
tree_ltl = head . fst . parse

main = do
        print $ display_lisp tree1
        print $ display_lisp tree2
        print $ display_lisp tree3
        print $ display_lisp tree4
        print $ display_lisp tree5
        print $ tree_ltl $ display_lisp tree5

