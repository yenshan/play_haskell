

_compress :: Eq a => [a] -> a -> [a] -> [a]
_compress [] _ res = res
_compress (x:xs) p res
   | x == p = _compress xs p res
   | otherwise = _compress xs x (res++[x])

compress (x:xs) = _compress xs x [x]
