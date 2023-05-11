import Data.List
import Data.Function

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

filled :: Tree a -> [[Bool]]
filled Empty = [[False]]
filled (Branch _ l r) = [True] : zipWith (++) (filled l) (filled r)   


type Pos = (Int, Int)
assignXY :: Tree a -> Int -> Int -> (Tree (a, Pos), Int)
assignXY Empty x d = (Empty, x)
assignXY (Branch v l r) x d = (Branch (v, (thisX, d)) left right, nextX)
    where (left, thisX) = assignXY l x (d+1)
          (right, nextX) = assignXY r (thisX+1) (d+1)

treeLayout :: Tree a -> Tree (a, Pos)
treeLayout t = fst $ assignXY t 1 1


findHeight :: Tree a -> Int
findHeight Empty = 0
findHeight (Branch _ l r) = 1 + max (findHeight l) (findHeight r)

-- Another tricky one. Our function takes as parameters the
-- height of the outer tree, a subtree of the outer tree, 
-- that tree's x postion and that tree's y position. The 
-- function then returns the updated tree (Tree (a, Pos)) 
-- as well as the x position of the parent according to the 
-- current tree.
--
-- The reason we're also returning the parent's x position
-- has to do with nodes in the tree not knowing what their
-- actual x position is until we've found the left most node
-- and labelled it with x=1. So once we do find that left most
-- node, its parent will be able to update their x positions
-- relative to that left most node. For all nodes that are not
-- an ascendant of the left most node we'll see that the x
-- position we return in the second part of the tuple is
-- actually just ignored.
--
-- Note: I've labelled the x distance between the left and right
--       subtrees of the current node as the 'diameter'. As such
--       the left subtree if it exists will have an x position of
--       currentX - radius, while the right subtree will have an
--       x position of currentX + radius.
--
-- So we start at the root node and just label it with an (x, y)
-- position of (1, 1) for now. Then we go left recursively (*)
-- until we reach a node that has an Empty left subtree. When we
-- call assignXY2 on that Empty subtree is going to give us back
-- (left, adjX) = (Empty, 1) indicating that its parent's x position
-- is 1. That node can now call assignXY2 on its right subtree
-- with an x position of (1 + radius), which will update the nodes
-- in the right subtree with their correct x position. Then it can
-- pass its parents position back up to its parent, (which until 
-- now didn't know its position relative to the left most node.
-- This happens all the way up the left side of the outer tree, 
-- back up to the root node, at which point the root node is now
-- sure of its x position and can call assignXY2 on its right
-- subtree with x position = rootX + radius. Once the recursion
-- completes there, we'll have our outer tree in which each node
-- contains not only the original value, but its (x, y) position too.
--
-- realX: this will be equal to adjX only in the left most node 
--        and all of its ascendants. They're the only ones that 
--        start out with an x position <= 1. 
assignXY2 :: Int -> Tree a -> Int -> Int -> (Tree (a, Pos), Int)
assignXY2 _ Empty x _ = (Empty, 1)
assignXY2 h (Branch v l r) x y = (Branch (v, (realX, y)) left right, realX + diameter)
          where (right, _)    = assignXY2 h r (realX + radius) (y+1)
                realX = max x adjX
                (left, adjX)  = assignXY2 h l (x-radius) (y+1) -- (*)
                radius        = diameter `div` 2
                diameter      = 2 ^ (h - y)

treeLayout2 :: Tree a -> Tree (a, Pos)
treeLayout2 t = fst $ assignXY2 h t 1 1
    where h = findHeight t

type VPos a = (a, (Int, Int))

positions :: Tree (VPos a) -> [Pos]
positions Empty = []
positions (Branch (_, p) l r) = p : (positions l ++ positions r)

xByLevel :: Tree (VPos a) -> [[Int]]
xByLevel Empty = []
xByLevel t = map (map fst) $ groupSnd $ sortSnd $ positions t 
    where groupSnd = groupBy (\a b -> snd a == snd b)
          sortSnd  = sortBy (compare `on` snd) 

leftMost :: Tree (VPos a) -> [Int]
leftMost t = map minimum $ xByLevel t  

rightMost :: Tree (VPos a) -> [Int]
rightMost t = map maximum $ xByLevel t

updateX :: Tree (VPos a) -> Int -> Tree (VPos a)
updateX Empty _ = Empty
updateX (Branch (v, (_, y)) l r) newX = Branch (v, (newX, y)) newL newR
    where newL = updateX l (newX-1)
          newR = updateX r (newX+1)

shiftBy :: Tree (VPos a) -> Int -> Tree (VPos a)
shiftBy Empty _ = Empty
shiftBy (Branch (v, (x, y)) l r) n = Branch (v, (x+n, y)) newL newR
    where newL = shiftBy l n
          newR = shiftBy r n

tighten :: Tree (VPos a) -> Tree (VPos a)
tighten (Branch v Empty Empty) = Branch v Empty Empty 
tighten t@(Branch (_, (x, _)) l Empty) = updateX t x 
tighten t@(Branch (_, (x, _)) Empty r) = updateX t x
tighten (Branch v l r) = Branch v (shiftBy l minGap) (shiftBy r (-minGap))
    where minGap = minimum $ map (\e -> (e - 1) `div` 2) gaps 
          gaps = [ y - x | (x, y) <- zip (rightMost l) (leftMost r)]

tightenRec :: Tree (VPos a) -> Tree (VPos a)
tightenRec Empty = Empty
tightenRec (Branch v l r) = tighten $ Branch v (tightenRec l) (tightenRec r)

lowestX :: Tree (VPos a) -> Int
lowestX (Branch (_, (x, _)) Empty _) = x
lowestX (Branch _ l _) = lowestX l 

-- The whole idea here was to take the layout function from q.65
-- and pull together or 'tighten' the subtrees starting from the leaves.
tightLayout :: Tree a -> Tree (VPos a)
tightLayout t = shiftBy tightTree (1 - offset) 
    where offset    = lowestX tightTree
          tightTree = tightenRec $ treeLayout2 t


