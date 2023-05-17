import Data.List

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])] deriving (Show, Eq)
data Friendly a = Edge [(a, a)] deriving (Show, Eq)

graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph nodes edges) = Adj $ map (getAdj edges) nodes
    where getAdj edges n = (n, foldr (\e acc -> if edgeHas e n then otherNode e n : acc else acc) [] edges)
          edgeHas (e1, e2) n = n == e1 || n == e2
          otherNode (e1, e2) x = if e1 == x then e2 else e1

adjToGraph :: (Eq a) => Adjacency a -> Graph a
adjToGraph (Adj as) = Graph nodes edges
    where nodes = as >>= pure . fst
          eqTuple (l, m) (n, o) = (l == n && m == o) || (l == o && m == n)
          toEdges (a, xs) = (a, ) <$> xs
          edges = nubBy eqTuple $ concatMap toEdges as

graphToHuman :: (Eq a) => Graph a -> Friendly a
graphToHuman (Graph nodes edges) = Edge (edges ++ zip iso iso)
    where iso = filter (`notElem` ([fst, snd] <*> edges)) nodes 

humanToGraph :: (Eq a, Ord a) => Friendly a -> Graph a
humanToGraph (Edge edges) = Graph nodes filteredEdges
    where nodes = sort $ nub $ [fst, snd] <*> edges
          samePair (a, b) (d, c) = (a == d && b == c) || (a == c && b == d)
          notIsoPair (a, b) = a /= b
          filteredEdges = filter notIsoPair $ nubBy samePair edges

