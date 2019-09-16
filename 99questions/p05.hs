

_myReverse :: [a] -> [a] -> [a]
_myReverse [] res = res
_myReverse (x:xs) res = _myReverse xs (x:res)

myReverse :: [a] -> [a]
myReverse xs = _myReverse xs []
