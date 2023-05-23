import Data.List


type Pos = (Int, Int)
type History = [Pos]

boardSize = 4
--moves' = [(1, 0), (-1, 0), (0, 1), (0, -1)]
moves' = [(1, 2), (2, 1), (-1, 2), (-2, 1), (1, -2), (2, -1), (-2, -1), (-1, -2)]

validPos :: Pos -> Bool
validPos (x, y) = x >= 0 && x < boardSize && y >= 0 && y < boardSize


addPos :: Pos -> Pos -> Pos
addPos (x1, y1) (x2, y2) = (x1+x2, y1+y2)

moves :: Pos -> [Pos]
moves p = filter validPos nexts
    where nexts = addPos p <$> moves'

isComplete :: History -> Bool
isComplete h = length h > boardSize ^ 2 && all (`elem` h) [(x, y) | x <- [0..boardSize-1], y <- [0..boardSize-1]]

shortest :: [[a]] -> [a]
shortest [] = []
shortest [x] = x
shortest (x:xs) = if length x < length y then x else y
    where y = shortest xs

search :: History -> Pos -> History
search h p  
    | isComplete (p:h) = p:h
    | length h > 18 = h
    | otherwise    = shortest $ map (search (p:h)) newMoves
        where newMoves = if null h then (moves p) else filter (/= head h) (moves p)

knightTour :: Pos -> History
knightTour = search []

branch :: [Pos] -> [[Pos]]
branch h@(x:xs) = map (:) (moves x) <*> [h] 

prune :: [[Pos]] -> [[Pos]]
prune ps = nubBy (\x y -> (nub . sort) x == (nub . sort) y) ps 

search2 :: [[Pos]] -> [[Pos]]
search2 xs = prune $ concatMap branch xs

helper :: [[Pos]] -> Int -> [[Pos]]
helper ps 0 = ps
helper ps n = helper (search2 ps) (n-1)

helper2 :: [[Pos]] -> [Pos]
helper2 ps = if any isComplete ps then head (filter isComplete ps) else helper2 (search2 ps)

kt2 :: Pos -> [[Pos]]
kt2 p = helper [[p]] 17

type PosCtx = ([Pos], [Pos])


insertCtx :: Pos -> [Pos] -> [Pos]
insertCtx p [] = [p]
insertCtx p ctx@(c:cs)
    | p == c = ctx
    | p > c  = c : insertCtx p cs
    | p < c  = p : ctx

removeCtx :: Pos -> [Pos] -> [Pos]
removeCtx _ [] = []
removeCtx p (c:cs)
    | p == c = cs
    | otherwise = c : removeCtx p cs

branch2 :: PosCtx -> [PosCtx]
branch2 (ps@(x:xs), ctx) = map (\a -> (a:ps, removeCtx a ctx)) (moves x)

prune2 :: [PosCtx] -> [PosCtx]
prune2 ps = nubBy (\x y -> snd x == snd y) ps

search3 :: [PosCtx] -> [PosCtx]
search3 xs = prune2 $ concatMap branch2 xs

helperCtx :: [PosCtx] -> Int -> [PosCtx]
helperCtx ps 0 = ps
helperCtx ps n = helperCtx (search3 ps) (n-1)

helperCtx2 :: [PosCtx] -> [Pos]
helperCtx2 ps = if not (null complete) then fst (head complete) else helperCtx2 (search3 ps)
    where complete = filter (\(a, b) -> isComplete a) ps

kt3 :: Pos -> [PosCtx]
kt3 p = helperCtx [([p], removeCtx p [(x, y) | x <- [0..boardSize-1], y <- [0..boardSize-1]])] 21
