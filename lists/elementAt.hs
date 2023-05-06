elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list has no elements"
elementAt (x:xs) n
    | n > length xs = error "Index to high"
    | n < 1         = error "Index must be positive"
    | n == 1        = x
    | otherwise     = elementAt xs (n-1)
