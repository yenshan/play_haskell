module Chess.FEN (parseFen) where

import Data.Char
import Data.List
import Data.List.Split

parseFen :: String -> String
parseFen fen = concat $ map (++"\n")$ zipWith (zipWith decode) board emptyBoard
    where
        bs = head $ words fen
        col = (head . tail) $ words fen
        board = map (concat . map convWhite) $ splitOn "/" $ if col == "w" then bs else reverse bs
        decode x y = if x == 'w' then y else uchar x


emptyBoard = [concat $ replicate 4 e | 
              i <- [0..7], 
              let e = if even i then [emptyWhite,emptyBlack] else [emptyBlack,emptyWhite]]

emptyWhite =  '\x2587'
emptyBlack =  '\xFF3F'

uchar 'K' = '\x265A'
uchar 'Q' = '\x265B'
uchar 'R' = '\x265C'
uchar 'B' = '\x265D'
uchar 'N' = '\x265E'
uchar 'P' = '\x265F'
uchar 'k' = '\x2654'
uchar 'q' = '\x2655'
uchar 'r' = '\x2656'
uchar 'b' = '\x2657'
uchar 'n' = '\x2658'
uchar 'p' = '\x2659'

convWhite n | n `elem` "12345678" = replicate (digitToInt n) 'w'
            | otherwise = [n]


testdat = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
testdat2 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
