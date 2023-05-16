data Tree a = Node a [Tree a]
        deriving (Eq, Show)

nnodes :: Tree a -> Int
nnodes (Node _ xs) = 1 + sum (map nnodes xs)

treeToString :: Tree Char -> String
treeToString (Node v xs) = v : concatMap treeToString xs ++ ['^'] 


subtreeStrings :: String -> [String]
subtreeStrings xs = map reverse . words . fst $ foldl helper ("", 0) xs
    where helper (a, t) x = if x == '^' then (if t == 1 then ' ':x:a else x:a, t-1) else (x:a, t+1)

stringToTree :: String -> Tree Char
stringToTree (x:xs) = Node x $ map stringToTree (init $ subtreeStrings xs)
