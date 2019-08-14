

myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs


myLast2 ::[a] -> a
myLast2 (x:xs) = if xs == [] then x else myLast xs
