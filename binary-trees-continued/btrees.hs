data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ l r) = 1 + countLeaves l + countLeaves r

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = x : (internals l ++ internals r)

internals' :: Tree a -> [a]
internals' t = intList t []
    where intList Empty xs = xs
          intList (Branch _ Empty Empty) xs = xs
          intList (Branch x l r) xs = x : intList l (intList r xs)

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x l r) n
    | n == 1    = [x]
    | otherwise = atLevel l (n-1) ++ atLevel r (n-1)

genTreeLT :: Int -> Int -> Tree Int
genTreeLT n mx
    | n > mx    = Empty
    | otherwise = Branch n (genTreeLT (2*n) mx) (genTreeLT (2*n+1) mx)

compBinTree :: Int -> Tree Int
compBinTree = genTreeLT 1

-- Solution to checking if a tree is a complete binary tree.
-- First we use this function to get a list of the depth of all
-- the nodes which have at least 1 Empty subtree. We do this from
-- left to right. 
getLeafDepths :: Tree a -> [Int]
getLeafDepths t = depths t 1 
    where depths Empty n = []
          depths (Branch _ Empty r) n = n : depths r (n+1)
          depths (Branch _ l Empty) n = depths l (n+1) ++ [n]
          depths (Branch _ l r) n = depths l (n+1) ++ depths r (n+1)

-- For a complete binary tree we would expect the semi-leaf nodes we
-- get above to be either all the same, meaning the tree is full to 
-- a specific depth, or, if the tree is not full, we should see some
-- nodes of depth n at the start of the list, and at some point we see
-- nodes with depth (n-1). Basically our list must satisfy two conditions:
-- (1) descending
-- (2) difference between any two elements in the list is <= 1
isCompBinTree :: Tree a -> Bool
isCompBinTree t = all (h - 1 ==) rest
    where (_, rest) = break (h /=) leaves
          h       = head leaves
          leaves  = getLeafDepths t
