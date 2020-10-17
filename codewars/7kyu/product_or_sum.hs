
import Data.List



productOrSum :: [Int] -> Int -> ProductOrSum
productOrSum xs n | s > p = Sum
                  | s < p = Product
                  | otherwise = Same
    where
        s = sum $ take n $ sortBy (flip compare) xs 
        p = foldl (*) 1 $ take n $ sort xs


data ProductOrSum = Product | Same | Sum deriving (Eq,Ord,Enum,Bounded,Show)


testdata :: [Int]
testdata = [10, 41, 8, 16, 20, 36, 9, 13, 20] 
