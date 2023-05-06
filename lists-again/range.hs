range :: Int -> Int -> [Int]
range a b
    | a == b = [a] 
    | a < b  = a : range (a+1) b 
    | a > b  = a : range (a-1) b

range2 :: Int -> Int -> [Int]
range2 a b =  scanl op a $ replicate (abs $ a - b) 1
    where op = if a <= b then (+) else (-)
