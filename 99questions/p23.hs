import System.Random
import Data.List


make_rnd_list :: StdGen -> [a] -> Int -> [a]
make_rnd_list  _ _ 0 = []
make_rnd_list seed xs n =
    let len = length xs
        (r1,gen1) = randomR (0,len-1) seed
    in xs!!r1 : make_rnd_list gen1 xs (n-1)
    
    
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do 
    gen0 <- newStdGen
    return (make_rnd_list gen0 xs n)

