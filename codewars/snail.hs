
array = [[1,2,3],
         [8,9,4],
         [7,6,5]]


toLeft (c,r) = (c,r-1)
toRight (c,r) = (c,r+1)
toDown (c,r) = (c+1,r)
toUp (c,r) = (c-1,r)

goRound sp 0 = [sp]
goRound sp dist = sp : rt ++ rd ++ ld ++ lu ++ next
    where
         rt = take dist $ iterate toRight (toRight sp)
         rd = take dist $ iterate toDown $ toDown (last rt)
         ld = take dist $ iterate toLeft $ toLeft (last rd)
         lu = take (dist-1) $ iterate toUp $ toUp (last ld) 
         next = if lu == [] then [] else goRound (toRight $ last lu) (dist-2)
    

snail array = map getVal route 
    where
        len = length array
        route = goRound (0,0) (len-1)
        getVal (r,c) = (concat array) !! (r*len+c)      
