

data Elem a = Single a | Multiple Int a deriving Show

_pack :: Eq a => [a] -> a -> [a] -> [[a]] -> [[a]]
_pack [] _ pac res = (res++[pac])
_pack (x:xs) p pac res
    | x == p = _pack xs p (pac++[x]) res
    | otherwise = _pack xs x [x] (res++[pac])

pack :: Eq a => [a] -> [[a]]
pack (x:xs) = _pack xs x [x] []

enc :: [a] -> Int -> Elem a
enc [x] 1 = Single x
enc [x] cnt = Multiple cnt x
enc (_:xs) cnt = enc xs (cnt+1)

encodeModified :: Eq a => [a] -> [Elem a] 
encodeModified xs = [enc x 1 | x <- pack xs]

