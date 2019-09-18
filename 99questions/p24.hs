import System.Random
import Data.List


make_rnd_list :: StdGen -> Int -> Int -> [Int]
make_rnd_list  _ _ 0 = []
make_rnd_list seed max n =
    let (r1,gen1) = randomR (1,max) seed
    in r1 : make_rnd_list gen1 max (n-1)
    
    
diff_select :: Int -> Int -> IO [Int]
diff_select n max = do 
    gen0 <- newStdGen
    return (make_rnd_list gen0 max n)

