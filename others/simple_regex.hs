--
-- 「ビューティフルコード」のカーニハン先生が紹介したロブ・パイクのコードに触発されて書いたミニマムな正規表現のmatch関数
-- 対応する正規表現：c . * ^ $
--   cは任意の文字
--

match_here :: [Char] -> [Char] -> Bool
match_here [] _ = True
match_here (x:xs) []
    | x == '$' && xs == [] = True
    | otherwise = False
match_here (x:xs) (y:ys)
    | x == y || x == '.' = if xs /= [] && head xs == '*' 
                           then match_repeat x (tail xs) ys
                           else match_here xs ys
    | otherwise = False


match_repeat :: Char -> [Char] -> [Char] -> Bool
match_repeat _ [] _ = True
match_repeat _ _ [] = False 
match_repeat c (x:xs) (y:ys)
    | x == y = match_here xs ys
    | c == y || c == '.' = match_repeat c (x:xs) ys
    | otherwise = match_here xs (y:ys)


-- match regex string 
match :: [Char] -> [Char] -> Bool
match _ [] = False
match (x:xs) (y:ys)
    | x == '^' = match_here xs (y:ys)
    | match_here (x:xs) (y:ys) = True
    | otherwise = match (x:xs) ys
