import Data.List

combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations 0 _ = []
combinations _ [] = []
combinations 1 xs = [[x] | x <- xs]
combinations n xs = nubBy eqComb $ concatMap mapComb $ zip [0..] xs
    where mapComb = \(i, x) -> map (x:) (cs i)
          cs i = combinations (n-1) (take i xs ++ drop (i+1) xs) 
          eqComb xs ys = all id [x `elem` ys | x <- xs] 
