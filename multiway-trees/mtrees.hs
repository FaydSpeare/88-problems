import Data.List

data Tree a = Node a [Tree a]
        deriving (Eq, Show)

nnodes :: Tree a -> Int
nnodes (Node _ xs) = 1 + sum (map nnodes xs)

treeToString :: Tree Char -> String
treeToString (Node v xs) = v : concatMap treeToString xs ++ ['^'] 

-- I think we could do this a little nicer, but on to the next...
subtreeStrings :: String -> [String]
subtreeStrings xs = map reverse . words . fst $ foldl helper ("", 0) xs
    where helper (a, t) x = if x == '^' then (if t == 1 then ' ':x:a else x:a, t-1) else (x:a, t+1)

stringToTree :: String -> Tree Char
stringToTree (x:xs) = Node x $ map stringToTree (init $ subtreeStrings xs)


-- Internal Path Length. Sum of the length from root to all nodes
ipl :: Tree a -> Int
ipl = helper 0
    where helper :: Int -> Tree a -> Int
          helper depth (Node _ nodes) = depth + sum (map (helper (depth+1)) nodes)


bottomUp :: Tree Char -> String
bottomUp (Node x nodes) = concatMap bottomUp nodes ++ [x]

-- more efficient bottomUp from the 99 haskell problems solutions
bottomUp' :: Tree Char -> String
bottomUp' tree = helper tree []
    where helper (Node x nodes) list = foldr helper (x:list) nodes 

displayLisp :: Tree Char -> String
displayLisp (Node x nodes) 
    | not (null nodes) = ['(', x, ' '] ++ unwords (map displayLisp nodes) ++ [')']
    | otherwise = [x]
