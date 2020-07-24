
import Data.List
import qualified Data.Map.Strict as M


f1 x = 2 * x + 1
f2 x = 3 * x + 1

makeSequence xs n seq
    | n < M.size seq && nth <= minimum nn = nth
    | otherwise = makeSequence nn n nseq
    where
        nth = fst $ M.elemAt n seq
        nn = concat [[f1 x, f2 x] | x <- xs]
        nseq = foldl (\m x -> M.insert x 1 m) seq nn


dblLinear n = makeSequence [1] (n-1) (M.fromList [])

main = do
        print $ dblLinear 6000
