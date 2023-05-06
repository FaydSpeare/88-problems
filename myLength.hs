myLength :: [a] -> Int
myLength = foldr (\_ acc -> acc + 1) 0 
