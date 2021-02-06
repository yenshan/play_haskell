
parseInt :: String -> Int
parseInt str = parse 0 wd
    where
       wd = words $ map (\c -> if c=='-' then ' ' else c) str

parse sum [] = sum
parse sum ("hundred":xs) = parse (sum*100) xs
parse sum ("thousand":xs) = sum*1000 + parse 0 xs
parse sum ("million":xs) = sum*1000000 + parse 0 xs
parse sum (x:xs) = parse (sum+parseNum x) xs


parseNum "thirty" = 30
parseNum "forty" = 40
parseNum "fifty" = 50
parseNum "sixty" = 60
parseNum "seventy" = 70
parseNum "eighty" = 80
parseNum "ninety" = 90
parseNum str = elemIndex 0 str ["zero","one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen","twenty"]


elemIndex _ _ [] = 0
elemIndex n str (x:xs) | str == x = n
                       | otherwise = elemIndex (n+1) str xs



dat = "two hundred forty-six" 
dat2 = "one thousand three hundred and thirty-seven"
dat3 = "two hundred four thousand eight hundred thirty-one"
