myGcd :: Int -> Int -> Int
myGcd a b
    | a `rem` b == 0 = abs b
    | otherwise      = myGcd b $ a `rem` b

coprime :: Int -> Int -> Bool
coprime a b = myGcd a b == 1

totient :: Int -> Int
totient 1 = 1
totient x = sum $ fromEnum . (coprime x) <$> [1..(x-1)]
