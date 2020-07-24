
import Data.Char


getScore '/' = 0
getScore 'X' = 10
getScore x = ord x - ord '0'


bscore_ [] = 0
bscore_ ['X',x1,x2] = 10 + bscore_ (x1:[x2])
bscore_ ('X':x1:x2:xs) = 10 + bscore_ (x1:[x2]) + bscore_ (x1:x2:xs)
bscore_ [x1,'/'] = 10
bscore_ [x1,'/',x2] = 10 + getScore x2
bscore_ (x1:'/':x2:xs) = 10 + getScore x2 + bscore_ (x2:xs)
bscore_ (x1:xs) = getScore x1 + bscore_ xs


bowlingScore myFrames = bscore_ $ filter (/=' ') myFrames

test1 = "11 11 11 11 11 11 11 11 11 11"
test2 = "X X X X X X X X X XXX"
test3 = "6/ 5/ 6/ 2/ 3/ 0/ 1/ 8/ 3/ 6/5"
test4 = "00 00 00 00 00 00 00 00 X 0/X"
