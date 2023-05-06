import Data.List

data MyCount a = None a | Single a | Multiple Int a deriving (Show)

incIfEq :: (Eq a) => a -> (MyCount a) -> (MyCount a)
incIfEq y (None x)
    | x == y    = Single x
    | otherwise = None x
incIfEq y (Single x)
    | x == y    = Multiple 2 x
    | otherwise = Single x
incIfEq y (Multiple n x)
    | x == y    = Multiple (n+1) x
    | otherwise = Multiple n x

countUnique :: (Eq a) => [a] -> [(MyCount a)]
countUnique xs = map (foldr (.) id functions) baseCounts
    where baseCounts = map (\x -> None x) $ nub xs
          functions  = map incIfEq xs

