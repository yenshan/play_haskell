

data Elem a = Single a | Multiple Int a deriving Show


decode :: Elem a -> [a]
decode (Single x) = [x]
decode (Multiple n x) = [x | _ <- [1..n]]

decodeModified :: [Elem a] -> [a] 
decodeModified [] = []
decodeModified (x:xs) = decode x ++ decodeModified xs
