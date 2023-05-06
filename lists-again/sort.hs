import Data.List
import Data.Function

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = (lsort smaller) ++ [x] ++ (lsort bigger)
    where smaller = filter (\y -> (length y) <= (length x)) xs
          bigger  = filter (\y -> (length y) > (length x)) xs


lfsort :: [[a]] -> [[a]]
lfsort xs = sortBy (compare `on` lengthFrequency) xs
    where lengthFrequency y = length $ [x | x <- xs, length y == length x ] 
