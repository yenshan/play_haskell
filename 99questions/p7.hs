
data NestedList a = Elem a | List [NestedList a]

flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten xs 
