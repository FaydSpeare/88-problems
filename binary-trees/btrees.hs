data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

-- This one was a bit tricky, but list comprehension is a great help.
-- In essence, we want to recursivly add subtrees while decreasing n.
-- Since we're dealing with binary trees, we need a left subtree and a
-- right subtree such that the sum of their nodes is equal to (n-1).
-- Additionally, we need to keep them balanced, so the difference
-- between the number of nodes in the subtrees is at most 1.
--
-- There are two cases:
-- (1) If (n-1) is even then the number of nodes in each subtree much be
-- equal. Otherwise their difference in number of nodes would exceed 1.
-- (2) If (n-1) is odd then there are two ways to pick the subtrees such
-- that our conditions hold. Consider the example with n=6. (n-1) = 5.
-- we could choose 2 for number of nodes in the left subtree and 3 for
-- the number of nodes in the right subtree or vice versa.
--
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n
    | even (n-1) = [Branch 'x' l r | l <- cbalTree fh, r <- cbalTree fh]
    | odd (n-1)  = [Branch 'x' l r | (y, z) <- xs, l <- cbalTree (fh + y), r <- cbalTree (fh + z)]
        where xs = [(0, 1), (1, 0)]
              fh = (n-1) `div` 2 -- Floor of half


isMirror :: Tree a -> Tree a -> Bool
isMirror Empty Empty = True
isMirror (Branch _ l1 r1) (Branch _ l2 r2) = isMirror l1 r2 && isMirror l2 r1
isMirror _ _ = False


isSymmetric :: Tree a -> Bool
isSymmetric Empty = True
isSymmetric (Branch _ l r) = isMirror l r


insertTree :: (Ord a) => Tree a -> a -> Tree a
insertTree Empty x = Branch x Empty Empty
insertTree (Branch y left right) x
    | x == y = Branch y left right
    | x < y  = Branch y (insertTree left x) right
    | x > y  = Branch y left (insertTree right x)


construct :: (Ord a) => [a] -> Tree a
construct = foldl insertTree Empty


maxHeightTree :: Int -> Tree Char
maxHeightTree 0 = Empty
maxHeightTree 1 = Branch 'x' Empty Empty
maxHeightTree n = Branch 'x' (maxHeightTree (n-1)) (maxHeightTree (n-2))

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ l r) = 1 + countNodes l + countNodes r

minNodes :: Int -> Int
minNodes n = countNodes . maxHeightTree $ n

