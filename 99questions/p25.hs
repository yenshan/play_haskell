import System.Random
import Data.List


deleteN :: [a] -> Int -> [a]
deleteN xs n = take n xs ++ drop (n+1) xs

make_rnd_permu :: Int -> [a] -> [a]
make_rnd_permu  _ [] = []
make_rnd_permu seed xs =
    let len = length xs
        (r1,gen1) = randomR (0,len-1) seed
    in xs!!r1 : make_rnd_permu gen1 (deleteN xs r1)
    
    
rnd_permu :: [a] -> IO [a]
rnd_permu xs = do 
    gen0 <- newStdGen
    return (make_rnd_permu gen0 xs)

