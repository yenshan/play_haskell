import Data.List


subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = subs xs ++ [ x : a | a <- subs xs]

delete_grp :: Eq a => [a] -> [a] -> [a]
delete_grp [] ys = ys
delete_grp (x:xs) ys = delete_grp xs (delete x ys)

mygroup :: Eq a => [Int] -> [a] -> [[[a]]]
mygroup [_] ys = [[ys]]
mygroup (n:ns) ys =  [ a : b | a <- subsn n ys, b <- mygroup ns (delete_grp a ys)]
                     where 
                        subsn n xs = [ a | a <- subs xs, length a == n]
        
main = do
        print $ mygroup [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
