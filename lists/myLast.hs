-- Find the last element in a list
myLast :: [a] -> a
myLast [] = error "Empty list has no last element"
myLast xs = xs !! (length xs -1) 

myLast2 :: [a] -> a
myLast2 [] = error "Empty list has no last element"
myLast2 [x] = x
myLast2 (_:xs) = myLast xs

myLast3 :: [a] -> a
myLast3 [] = error "Empty list has no last element"
myLast3 (x:xs) = foldl (\acc x -> x) x xs 
