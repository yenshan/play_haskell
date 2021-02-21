
kingIsInCheck :: [[Char]] -> Bool
kingIsInCheck xs = any (==True) $ map (checkKing xs) wps
    where
        posmap = [ (getC xs pos, pos) | x <- [0..7], y <- [0..7], let pos = [x,y]]
        wps = filter (\(c,_) -> c /= ' ' && c /= 'K') posmap


getC m [x,y] | not (inRange [x,y]) = ' ' 
             | otherwise = (m!!y)!!x

checkKing m ('P',[x,y]) = any (==True) $ map (isKing m) [[x-1,y+1],[x+1,y+1]]
checkKing m ('N',[x,y]) = any (==True) $ map (isKing m) [[x-2,y-1],[x-1,y-2],[x+1,y-2],[x+2,y-1],[x-1,y+2],[x-2,y+1],[x+1,y+2],[x+2,y+1]]
checkKing m ('Q',p) = checkKing m ('B',p) || checkKing m ('R',p)
checkKing m ('R',p) = checkRoute m p [routeLeft, routeRight, routeUp, routeDown]
checkKing m ('B',p) = checkRoute m p [routeLeftUp, routeRightUp, routeLeftDown, routeRightDown]
checkKing _ _ = False

isKing m p = getC m p == 'K'

checkRoute m p = any (==True) . map (checkKingOnRoute m) . map (\f -> f p)
    
checkKingOnRoute _ [] = False
checkKingOnRoute m (x:xs) | isKing m x = True 
                          | getC m x /= ' ' = False
                          | otherwise = checkKingOnRoute m xs

route [dx,dy] [x,y] | not (inRange [x,y]) = []
                    | otherwise = [nx,ny] : route [dx,dy] [nx,ny]
    where
        nx = x+dx
        ny = y+dy    

inRange [x,y] = x >= 0 && x <= 7 && y >= 0 && y <= 7

routeLeft = route [-1,0]
routeRight = route [1,0]
routeUp = route [0,-1]
routeDown = route [0,1]
routeLeftUp = route [-1,-1]
routeRightUp = route [1,-1]
routeLeftDown = route [-1,1]
routeRightDown = route [1,1]


data0 = [ "        "
        , "        "
        , "        "
        , "        "
        , " N      "
        , "        "
        , "K       "
        , "        "
        ]


data1 = [ "        "
        , "        "
        , "        "
        , "        "
        , "        "
        , " P      "
        , "K       "
        , "        "
        ]
data2 = [ "       K"
        , "        "
        , "        "
        , "        "
        , "        "
        , "        "
        , "        "
        , "B       "
        ]

