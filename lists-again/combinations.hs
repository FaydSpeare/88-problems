import Data.List

-- This is pretty terrible, since we end up generating all the permuations
-- and then creating a unique list from that with nub and function that
-- checks if two permutations have the all the same elements.
combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations 0 _ = []
combinations _ [] = []
combinations 1 xs = [[x] | x <- xs]
combinations n xs = nubBy eqPerm $ concatMap mapComb $ zip [0..] xs
    where mapComb = \(i, x) -> map (x:) (cs i)
          cs i = combinations (n-1) (take i xs ++ drop (i+1) xs) 
          eqPerm xs ys = all id [x `elem` ys | x <- xs] 

-- We can improve on the above quite easily by not generating a list of
-- permutations in the first place with a little trick. We essentially
-- just get rid of the take in the above solution.
betterCombs :: (Eq a) => Int -> [a] -> [[a]]
betterCombs 0 _ = [[]]
betterCombs n xs = concatMap mapComb $ zip [0..] xs
    where mapComb = \(i, x) -> map (x:) $ betterCombs (n-1) $ drop (i+1) xs


