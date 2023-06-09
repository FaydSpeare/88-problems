isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs)
    | x == last xs = isPalindrome $ init xs
    | otherwise    = False

isPalindrome2 :: (Eq a) => [a] -> Bool
isPalindrome2 = (==) <$> id <*> reverse
