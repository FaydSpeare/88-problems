removeAt :: [a] -> Int -> (a, [a])
removeAt xs n
    | outOfBounds = error "Index out of bounds"
    | otherwise   = (xs !! n, map fst filtered)
        where outOfBounds = n < 0 || n > length xs - 1 
              filtered    = filter (\z -> snd z /= n) $ zip xs [0..]

removeAt2 :: [a] -> Int -> (a, [a])
removeAt2 xs n
    | outOfBounds = error "Index out of bounds"
    | otherwise   = (xs !! n, (take n xs) ++ (drop (n+1) xs))
        where outOfBounds = n < 0 || n > length xs - 1 
