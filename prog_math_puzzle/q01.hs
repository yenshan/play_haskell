
numlist :: Int -> Int -> [Int]
numlist 0 _ = []
numlist n base = numlist (n `quot` base) base ++ [(n `mod` base)]

is_palendrome :: Int -> Int -> Bool
is_palendrome n base =
    nl == reverse nl
    where
        nl = numlist n base

palen_seq :: [Int]
palen_seq = [a | a <- [11,13..], 
                 is_palendrome a 10,
                 is_palendrome a 8,
                 is_palendrome a 2]

main :: IO ()
main = do
         print $ take 1 palen_seq

