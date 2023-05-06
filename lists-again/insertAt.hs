insertAt :: a -> [a] -> Int -> [a]
insertAt y xs n = (take (n-1) xs) ++ [y] ++ (drop (n-1) xs)
