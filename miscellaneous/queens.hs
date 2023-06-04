

sameDiag :: (Int, Int) -> (Int, Int) -> Bool
sameDiag (a, b) (c, d) = abs (a - c) == abs (b - d)

onSafeDiag :: [(Int, Int)] -> (Int, Int) -> Bool
onSafeDiag queens x = not $ any (sameDiag x) queens

safePositions :: [Int] -> [Int]
safePositions queens = [x | x <- safeRows, onSafeDiag queenPositions (x, y)]
    where safeRows = filter (`notElem` queens) [0..7]
          queenPositions  = zip queens [0..]
          y = length queens

queens :: Int -> [[Int]]
queens n = queensGivenExisting [[]] n
    where queensGivenExisting qs n
            | n == 0    = qs
            | otherwise = queensGivenExisting newQs (n-1)
                where branch xs = [xs ++ [q] | q <- safePositions xs]  
                      newQs     = concatMap branch qs
