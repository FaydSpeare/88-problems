import Data.Function (on)
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes, listToMaybe)

type Pos = (Int, Int)
type Moves = [Pos]
type PosSet = Set Pos

boardSize = 8 
moves' = [(1, 2), (2, 1), (-1, 2), (-2, 1), (1, -2), (2, -1), (-2, -1), (-1, -2)]
fullBoard = [(x, y) | x <- [0..boardSize-1], y <- [0..boardSize-1]]

validPos :: Pos -> Bool
validPos (x, y) = x >= 0 && x < boardSize && y >= 0 && y < boardSize

addPos :: Pos -> Pos -> Pos
addPos (x1, y1) (x2, y2) = (x1+x2, y1+y2)

moves :: Pos -> [Pos]
moves p = filter validPos nexts
    where nexts = addPos p <$> moves'

isComplete :: Moves -> Bool
isComplete h = length h >= boardSize ^ 2 && all (`elem` h) fullBoard

unvisitedMoves :: PosSet -> Pos -> Moves
unvisitedMoves unvisited = filter (`Set.member` unvisited) . moves

heuristic :: PosSet -> Pos -> Int
heuristic ms p = length (unvisitedMoves ms p)

sortByHeuristic :: PosSet -> Moves -> Moves
sortByHeuristic ms = sortBy (compare `on` heuristic ms)

heuristicMoves :: PosSet -> Pos -> Moves
heuristicMoves ms p = sortByHeuristic ms $ unvisitedMoves ms p

maybePath :: Pos -> Maybe Moves -> Maybe Moves
maybePath _ Nothing = Nothing
maybePath p (Just ms) = Just (p:ms)

findPathFrom :: PosSet -> Pos -> Maybe Moves
findPathFrom rem p
    | Set.null rem = Just [p]
    | otherwise     = maybePath p . listToMaybe . catMaybes $ paths
        where nextMoves = heuristicMoves rem p
              paths     = map (\m -> findPathFrom (Set.delete m rem) m) nextMoves

tour :: Pos -> Maybe Moves
tour p = findPathFrom (Set.fromList rem) p
    where rem = delete p [(x, y) | x <- [0..boardSize-1], y <- [0..boardSize-1]]


