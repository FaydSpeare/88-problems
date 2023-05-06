rotate :: [a] -> Int -> [a]
rotate xs n = (drop m xs) ++ (take m xs)
    where m = n `mod` length xs
