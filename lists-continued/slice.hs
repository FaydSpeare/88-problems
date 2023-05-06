slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs start end = let w = end - start
                     in take w $ drop start xs  
