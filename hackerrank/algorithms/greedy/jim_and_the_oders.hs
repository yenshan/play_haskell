--
--https://www.hackerrank.com/challenges/jim-and-the-orders/problem
--
import Data.List

jimOrders orders = 
        map fst (sortOn snd idx_sums)
        where 
            sums = [a+b | [a,b] <- orders]
            idx_sums = [(i+1, sums!!i) | i <- [0..(length sums)-1]]

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    n_tmp <- getLine
    let n = read n_tmp :: Int

    ordersTemp <- readMultipleLinesAsStringArray n
    let orders = Data.List.map (\x -> Data.List.map (read :: String -> Int) . words $ x) ordersTemp

    let result = jimOrders orders

    putStrLn $ intercalate " " $ Data.List.map (\x -> show x) $ result

