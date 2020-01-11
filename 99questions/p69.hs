
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)


anyChar :: [Char] -> (Char, [Char])
anyChar [] = (' ', "")
anyChar [x] = (x, "")    
anyChar (x:xs) = (x, xs)


ds2tree [] = (Empty, "")
ds2tree str =
    let (c1,str1) = anyChar str
        (c2,str2) = anyChar str1
        (c3,str3) = anyChar str2
    in    
    if c1=='.' then
        (Empty, str1)
    else
        if c2=='.' && c3=='.' then
           (Branch c1 Empty Empty, str3)
        else
            let (tl,str4) = ds2tree str1  
                (tr,str5) = ds2tree str4
            in (Branch c1 tl tr, str5)

tree2ds Empty = "."
tree2ds (Branch x l r) = [x] ++ tree2ds l ++ tree2ds r

example = "abd..e..c.fg..."

main = do
         print $ fst (ds2tree example)
         print $ tree2ds (Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty))
         return ()

    

