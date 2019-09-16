
_pack :: Eq a => [a] -> a -> [a] -> [[a]] -> [[a]]
_pack [] _ pac res = (res++[pac])
_pack (x:xs) p pac res
    | x == p = _pack xs p (pac++[x]) res
    | otherwise = _pack xs x [x] (res++[pac])

pack :: Eq a => [a] -> [[a]]
pack (x:xs) = _pack xs x [x] []
