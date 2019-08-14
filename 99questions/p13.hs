

data Elem a = Single a | Multiple Int a deriving Show

_encode :: Eq a => [a] -> a -> Int -> [Elem a] -> [Elem a]
_encode [] p cnt res = res++[Multiple cnt p]
_encode (x:xs) p cnt res
    | x == p = _encode xs p (cnt+1) res
    | cnt == 1 = _encode xs x 1 (res++[Single p])
    | otherwise = _encode xs x 1 (res++[Multiple cnt p])

encodeModified :: Eq a => [a] -> [Elem a] 
encodeModified (x:xs) = _encode xs x 1 []
