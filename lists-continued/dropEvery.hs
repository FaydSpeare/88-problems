split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs n = (take n xs, drop n xs) 
