module Kata where

dative :: String -> String
dative word 
    | elem lastV fronV = word ++ "nek"
    | elem lastV backV = word ++ "nak"
    | otherwise = word
    where
        lastV = last $ filter isVowel word


isVowel s = elem s (frontV++backV)


frontV = "eéiíöőüű"
backV = "aáoóuú"

testData = "tükör"
