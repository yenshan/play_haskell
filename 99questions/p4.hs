
_countLength [] n = n
_countLength (x:xs) n = _countLength xs (n+1)

myLength xs = _countLength xs 0
