isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | n == 2 = True
    | n > 2  = not $ 0 `elem` (map (n `mod`) [2..(n-1)])

isPrime' :: Int -> Bool
isPrime' n
    | n <= 1 = False
    | n == 2 = True
    | n > 2  = all (0 /=) $ [(n `mod`)] <*> [2..(n-1)]
