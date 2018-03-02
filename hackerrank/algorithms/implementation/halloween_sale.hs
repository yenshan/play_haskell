import Control.Applicative
import Control.Monad
import System.IO


price_list p d m = [price] ++ price_list (p - d) d m
                   where
                     price = if p <= m then m else p

num_can_buy sum cnt s (x:xs) 
    | (sum + x) > s = cnt 
    | otherwise = num_can_buy (sum + x) (cnt + 1) s xs


main :: IO ()
main = do
    p_temp <- getLine
    let p_t = words p_temp
    let p = read $ p_t!!0 :: Int
    let d = read $ p_t!!1 :: Int
    let m = read $ p_t!!2 :: Int
    let s = read $ p_t!!3 :: Int
    print $ num_can_buy 0 0 s (price_list p d m)
