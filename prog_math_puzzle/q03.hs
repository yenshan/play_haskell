
data Card = Front | Back deriving (Show,Eq)

_reverse :: Card -> Card
_reverse Front = Back
_reverse Back = Front

reverse_cards :: [(Int,Card)] -> Int -> [(Int,Card)]
reverse_cards xs n = 
    map rev_if xs
    where 
        rev_if (i,d) = if i `mod` n == 0
                       then (i, _reverse d)  
                       else (i,d)

do_reverse xs n
    | n > 100 = xs
    | xs == reverse_cards xs n = xs
    | otherwise = do_reverse (reverse_cards xs n) (n+1)

main :: IO ()
main = do
         let cards = [(i, Back) | i <-[1..100]]
         print $ [i | (i,d) <- do_reverse cards 2, d == Back]
         
