
import Data.List

distributeGifts :: String -> Either String Int
distributeGifts m | sp == (-1,-1) = Left "Where is Santa Claus?"
                  | length cp == 0 = Right 0
                  | otherwise = Right $ sumDistance (sp:cp)
    where
      mm = intercalate "," $ lines m
      cp = [ p |  c <- ['A'..'Z'], let p = findC c 0 0 mm, p /= (-1,-1)]
      sp = findC 's' 0 0 mm


sumDistance [_] = 0
sumDistance (x:y:xs) = distance x y + sumDistance (y:xs)
    where
        distance (a,b) (c,d) = abs (a-c) + abs (b-d)

findC :: Char -> Int -> Int -> [Char] ->(Int,Int)
findC _ x y [] = (-1,-1)
findC c x y (',':ms) = findC c 0 (y+1) ms
findC c x y (m:ms) | c == m = (x,y)
                   | otherwise = findC c (x+1) y ms




data1 =  ".....A....\n..s.......\n..........\n....C.....\n......B...\n"



