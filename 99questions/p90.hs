import Debug.Trace



attack (cx,rx) (cy,ry)
    | cx == cy = True
    | rx == ry = True
    | abs (cx-cy) == abs (rx-ry) = True
    | otherwise = False    


elemAttack x [] = False
elemAttack x (y:ys)
    | attack x y = True
    | otherwise = elemAttack x ys


queen_ c n qs
    | c == n = qp
    | otherwise = concat [queen_ (c+1) n nqs | nqs <- qp]
    where
        qp = [ qs ++ [(c,r)] | r <- [1..n], not $ elemAttack (c,r) qs]

queen n = map getQpos $ queen_ 1 n []
    where
        getQpos = map snd


main = do
         print $ length (queen 8)
         print $ head (queen 8)
