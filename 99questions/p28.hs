import Data.List
import Data.Ord


--
-- (a)
--

lsort xs = map snd $ sortBy (comparing fst) (addlen xs)
           where
             addlen ys = map (\x -> (length x,x)) ys


--
-- (b)
--

countElem a = length . filter (==a)

freqSort xs = map fst $ sortBy (comparing snd) [(a, countElem a xs) | a <- nub xs]

sortByP [] _ = []
sortByP (x:xs) ys = [a | a <- ys, length a == x] ++ sortByP xs ys

lfsort xs = sortByP plist xs
            where
              plist = freqSort $ map (length) xs



main = do
   print $ lsort ["abc","de","fgh","de","ijkl","mn","o"]
   print $ lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
