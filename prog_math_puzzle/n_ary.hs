import Data.Char

n_ary :: Int -> Int -> [Int]
n_ary 0 _ = []
n_ary num base = n_ary quotient base ++ [remainder]
                 where
                    quotient = num `quot` base
                    remainder = num `mod` base

n_ary_str :: Int -> Int -> String
n_ary_str num base = map intToDigit (n_ary num base)
