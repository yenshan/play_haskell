

_encode :: Eq a => [a] -> a -> Int -> [(Int,a)] -> [(Int,a)]
_encode [] p cnt res = res++[(cnt,p)]
_encode (x:xs) p cnt res
    | x == p = _encode xs p (cnt+1) res
    | otherwise = _encode xs x 1 (res++[(cnt,p)])

encode :: Eq a => [a] -> [(Int,a)] 
encode (x:xs) = _encode xs x 1 []


_pack :: Eq a => [a] -> a -> [a] -> [[a]] -> [[a]]
_pack [] _ pac res = (res++[pac])
_pack (x:xs) p pac res
    | x == p = _pack xs p (pac++[x]) res
    | otherwise = _pack xs x [x] (res++[pac])

pack :: Eq a => [a] -> [[a]]
pack (x:xs) = _pack xs x [x] []

enc :: [a] -> Int -> (Int,a)
enc [x] cnt = (cnt,x)
enc (x:xs) cnt = enc xs (cnt+1)

encode' :: Eq a => [a] -> [(Int,a)] 
encode' xs = [enc x 1 | x <- pack xs]
