


_encode [] p cnt res = res++[(cnt,p)]
_encode (x:xs) p cnt res
    | x == p = _encode xs p (cnt+1) res
    | otherwise = _encode xs x 1 (res++[(cnt,p)])

encode (x:xs) = _encode xs x 1 []
