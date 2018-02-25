;;
;; https://www.hackerrank.com/challenges/hackerland-radio-transmitters/problem
;;
import Control.Applicative
import Control.Monad
import System.IO
import Data.List


max_distance :: Int -> Int -> [Int] -> Int
max_distance a k xs = maximum ([a] ++ nums)
                      where
                          nums = takeWhile (\x -> (x - a) <= k) xs 

num_of_radio :: Int -> [Int] -> Int
num_of_radio k [] = 0
num_of_radio k (x:xs) = 1 + num_of_radio k remain
                        where
                            fp = max_distance x k xs
                            remain = dropWhile (\y -> y <= (fp + k)) xs

main :: IO ()
main = do
    n_temp <- getLine
    let n_t = words n_temp
    let n = read $ n_t!!0 :: Int
    let k = read $ n_t!!1 :: Int
    x_temp <- getLine
    let x = map read $ words x_temp :: [Int]
    print $ (num_of_radio k (sort x))


