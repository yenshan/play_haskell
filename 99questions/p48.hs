
infixl 3 `equ'`

(and') True True = True
(and') False _ = False
(and') _ False = False

(or') True _ = True
(or') _ True = True
(or') False False = False

(nand') a b = not (and' a b)

(nor') a b = not (or' a b)

(xor') True True = False
(xor') False False = False
(xor') _ _ = True


(impl') True True = True
(impl') True False = False
(impl') False True = True
(impl') False False = True

(equ') True True = True
(equ') False False = True
(equ') _ _ = False

combination 0 = [[]]
combination n = [ a : b | a <- [True,False], b <- combination (n-1)]

showa [] = ""
showa (x:xs) = show x ++ " " ++ showa xs

tablen n f = do
               mapM (\x -> putStrLn $ showa x ++ show (f x)) (combination n)  
               return ()

main = do
        tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
        return ()
