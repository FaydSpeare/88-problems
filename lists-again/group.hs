combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = concatMap mapComb $ zip [0..] xs
    where mapComb = \(i, x) -> map (x:) $ combinations (n-1) $ drop (i+1) xs

-- Not the most efficient as we'll be filtering out a lot, but gets the job done
group :: (Eq a) => (Int, Int, Int) -> [a] -> [[[a]]]
group (a, b, c) xs = filter eachPersonOnce $ [[x, y, z] | x <- twos, y <- threes, z <- fours]
    where twos   = combinations a xs
          threes = combinations b xs
          fours  = combinations c xs
          eachPersonOnce = (\w -> all (`elem` (concat w)) xs)
