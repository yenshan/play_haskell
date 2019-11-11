
and' True True = True
and' False _ = False
and' _ False = False

or' True _ = True
or' _ True = True
or' False False = False

nand' a b = not (and' a b)

nor' a b = not (or' a b)

xor' True True = False
xor' False False = False
xor' _ _ = True


impl' True True = True
impl' True False = False
impl' False True = True
impl' False False = True

eqal' True True = True
eqal' False False = True
eqal' _ _ = False

table f = do
            putStrLn $ "True True " ++ show (f True True)
            putStrLn $ "True False " ++ show (f True False)
            putStrLn $ "False True " ++ show (f False True)
            putStrLn $ "False False " ++ show (f False False)  
            return ()

main = do
        table (\a b -> (a `and'` (a `or'` not b)))
        return ()
