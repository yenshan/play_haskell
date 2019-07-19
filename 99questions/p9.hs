
_pack [] _ pac res = (res++[pac])
_pack (x:xs) p pac res
    | x == p = _pack xs p (pac++[x]) res
    | otherwise = _pack xs x [x] (res++[pac])

pack (x:xs) = _pack xs x [x] []
