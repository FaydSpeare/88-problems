pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = reverse $ foldl (\(a:as) x -> if head a == x then (x:a):as else [x]:a:as) [[x]] xs 

pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) = (x : takeWhile (==x) xs) : pack2 (dropWhile (==x) xs)

pack3 :: (Eq a) => [a] -> [[a]]
pack3 [] = []
pack3 (x:xs) = let (first, rest) = span (==x) xs
               in (x:first) : pack rest

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack
