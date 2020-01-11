
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)


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

-----------------------------
-----------------------------

treeToPreorder Empty = ""
treeToPreorder (Branch x l r) = [x] ++ treeToPreorder l ++ treeToPreorder r 

treeToInorder Empty = ""
treeToInorder (Branch x l r) = treeToInorder l ++ [x] ++ treeToInorder r


preInTree [] [] = Empty
preInTree (x:xs) io = Branch x (preInTree lp li) (preInTree rp ri)
                      where
                        li = takeWhile (/=x) io
                        ri = tail $ dropWhile (/=x) io
                        lp = take (length li) xs
                        rp = drop ((length xs)-(length ri)) xs
test1 = do
          t <- stringToTree "a(b(d,e),c(,f(g,)))"
          let po = treeToPreorder t
              io = treeToInorder t
          print $ preInTree po io
          return ()



