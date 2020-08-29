
import Data.List
import Data.Char

chessBoard = concat $ take 8 $ [concat $ replicate 4 (if even r then [0,1] else [1,0]) | r <- [0..]]

getIndex [c,n] = (elemIndex c "ABCDEFGH", (digitToInt n) - 1)

cellVal cell = chessBoard !! idx
    where
        (Just c,r) = getIndex cell
        idx = r * 8 + c

sameColor :: String -> String -> Bool
sameColor c1 c2 = cellVal c1 == cellVal c2
