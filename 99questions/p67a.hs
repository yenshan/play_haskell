
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

tree65 = Branch 'n'
                (Branch 'k'
                    (Branch 'c'
                        (Branch 'a' Empty Empty)
                        (Branch 'e'
                            (Branch 'd' Empty Empty)
                            (Branch 'g' Empty Empty)
                        )
                    )
                    (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                    (Branch 'p'
                        Empty
                        (Branch 'q' Empty Empty)
                    )
                    Empty
                )
                
anyChar :: [Char] -> (Char, [Char])
anyChar [] = (' ', "")
anyChar [x] = (x, "")    
anyChar (x:xs) = (x, xs)


parseString :: Monad m => [Char] -> m (Tree Char, [Char])
parseString [] = return (Empty, "")
parseString str = do
    let (c1,str1) = anyChar str
        (c2,str2) = anyChar str1
    if c1==',' || c1==')' then
        return (Empty, str1)
    else
        case c2 of
         c | c==',' || c==')' -> return (Branch c1 Empty Empty, str2)
           | c=='(' -> do 
                        (lt, str3) <- parseString str2
                        if str3/="" then
                          do
                            (rt, str4) <- parseString str3
                            let (k3,str5) = anyChar str4
                            return (Branch c1 lt rt, str5)
                        else
                            return (Empty, "")
           | otherwise -> return (Empty,"")

stringToTree :: Monad m => [Char] -> m (Tree Char)
stringToTree str = do
                    (t,_) <- parseString str
                    return t 


treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) = [x] ++ "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"


test1 = stringToTree "x(y,a(,b))" >>= print

test2 = do
          t <- stringToTree $ treeToString tree65
          print $ (t == tree65)
          return ()


