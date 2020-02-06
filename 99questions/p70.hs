
data Tree a = Node a [Tree a] deriving (Eq,Show)

tree5 = Node 'a' [
                    Node 'f' [Node 'g' []],
                    Node 'c' [],
                    Node 'b' [Node 'd' [], Node 'e' []]
                 ]

string5 = "afg^^c^bd^e^^^"

anyChar :: [Char] -> (Char, [Char])
anyChar [] = (' ',"")
anyChar [x] = (x,"")
anyChar (x:xs) = (x,xs)


treeToString :: Tree Char -> [Char]
treeToString (Node a []) = [a,'^']
treeToString (Node a xs) = [a] ++ concat [treeToString x | x <- xs] ++ ['^']



parse :: String -> ([Tree Char],String)
parse [] = ([],"")   
parse str = 
    let (c1,str1) = anyChar str
        (c2,str2) = anyChar str1
    in
    if c1=='^' then
        ([],str1)
    else
      if c2=='^' then
        let (t,str3) = parse str2
        in ([Node c1 []] ++ t,str3)
      else
        let (t,str3) = parse str1
        in 
          if str3 == "" then
            ([Node c1 t], str3)
          else
            let (t2,str4) = parse str3
            in ([Node c1 t] ++ t2, str4)


stringToTree  = head . fst . parse

main = do
        print tree5
        print $ treeToString tree5 
        print $ stringToTree $ treeToString tree5

