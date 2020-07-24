

{-
rayGo x y vx vy maxX maxY
    | x == 0 && y == 0 = [(x,y)]
    | x == 0 && y == maxY = [(x,y)]
    | x == maxX && y == 0 = [(x,y)]
    | x == maxX && y == maxY = [(x,y)]
    | x+vx > maxX || x+vx < 0 = rayGo x y (-vx) vy maxX maxY
    | y+vy > maxY || y+vy < 0 = rayGo x y vx (-vy) maxX maxY
    | otherwise = rayGo (x+vx) (y+vy) vx vy maxX maxY
-}

rayGo x y vx vy maxX maxY
    | x == 0 && y == 0 = True
    | x == 0 && y == maxY = False
    | x == maxX && y == 0 = False
    | x == maxX && y == maxY = True
    | x+vx > maxX || x+vx < 0 = rayGo x y (-vx) vy maxX maxY
    | y+vy > maxY || y+vy < 0 = rayGo x y vx (-vy) maxX maxY
    | otherwise = rayGo (x+vx) (y+vy) vx vy maxX maxY

reflections maxX maxY = rayGo 1 1 1 1 maxX maxY
