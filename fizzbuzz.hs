fizzBuzz :: Int -> [(Int, String)] -> String
fizzBuzz x xs = concat $ xs >>= aux
    where aux (a, b) = if x `mod` a == 0 then pure b else []

