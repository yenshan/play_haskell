--
-- https://www.hackerrank.com/challenges/closest-numbers/problem
--
import Data.List
import Data.Function
import Data.Ord


part (x:xs) 
    | xs == [] = []
    | otherwise = (x,head xs) : part xs

closestNumbers xs =
    concat [[a,b] | [s,a,b] <- sort_vals, s == minival]
    where
      difs = map (\x -> let (a,b) = x in [b-a,a,b]) $ (part.sort) xs
      sort_vals = sortBy (comparing head) difs
      minival = (head.head) sort_vals 
    

main :: IO()
main = do
    n <- readLn :: IO Int
    arrTemp <- getLine
    let arr = map (read :: String -> Int) . words $ arrTemp

    let result = closestNumbers arr

    putStrLn $ intercalate " " $ map (\x -> show x) $ result


