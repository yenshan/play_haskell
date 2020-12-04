
import Data.List
import Data.Ord


decodeBits :: String -> String
decodeBits bits = concat $ map (decode unit) dats
    where
        dats = decode' 0 $ trimzero bits
        unit = snd $ minimumBy (comparing snd) dats

trimzero xs = dropWhile (=='0') $ reverse $ dropWhile (=='0') $ reverse xs

decode :: Int -> (Char,Int) -> String
decode u ('0',n) | n == u = ""
                 | n == u*3 = " " 
                 | n == u*7 = "   "
                 | otherwise = "."
decode u ('1',n) | n == u = "."
                 | n == u*3 = "-"
                 | otherwise = "."

decode' :: Int -> String -> [(Char, Int)]
decode' cnt [x] = [(x,cnt+1)]
decode' cnt (x:y:xs) | x == y = decode' (cnt+1) (y:xs) 
                     | otherwise = (x, cnt+1) : decode' 0 (y:xs)


testdata = "1100110011001100000011000000111111001100111111001111110000000000000011001111110011111100111111000000110011001111110000001111110011001100000011"



answer = ".... . -.--   .--- ..- -.. ."







