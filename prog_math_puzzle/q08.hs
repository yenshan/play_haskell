
type Pos = (Int,Int)

directions :: Pos -> [Pos]
directions (x,y) = [(x,y-1),(x,y+1),(x-1,y),(x+1,y)]

move :: [Pos] -> Int -> Int
move xs 0 = 1
move (x:xs) n = sum [ move (d:x:xs) (n-1) | d <- directions x, not (elem d xs)]

main :: IO ()
main = do print $ move [(0,0)] 12

