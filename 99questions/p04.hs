
_countLength :: [a] -> Int -> Int
_countLength [] n = n
_countLength (x:xs) n = _countLength xs (n+1)

myLength :: [a] -> Int
myLength xs = _countLength xs 0
