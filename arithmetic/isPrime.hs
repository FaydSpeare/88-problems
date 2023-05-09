import Data.List

isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | n == 2 = True
    | n > 2  = not $ 0 `elem` (map (n `mod`) [2..(n-1)])

isPrime' :: Int -> Bool
isPrime' n
    | n <= 1 = False
    | n == 2 = True
    | n > 2  = all (0 /=) $ [(n `mod`)] <*> [2..(n-1)]

primeFact :: Int -> [Int]
primeFact 1 = []
primeFact n = a : primeFact (n `div` a)
    where a = head $ filter divides $ filter isPrime' [2..]
          divides x = n `mod` x == 0 

primeFactMult :: Int -> [(Int, Int)]
primeFactMult n = (\x -> (head x, length x)) <$> group (primeFact n)

impTotient :: Int -> Int
impTotient n = product [ formula x | x <- primeFactMult n ]
    where formula (y, z) = (y - 1) * (y) ^ (z - 1)

primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime' [a..b]

goldbach :: Int -> (Int, Int)
goldbach n = head $ [(x, y) | x <- ps, y <- ps, x + y == n]
    where ps = primesR 0 n

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach [x | x <- [a..b], even x]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b c = filter (\(x, y) -> x > c && y > c) $ goldbachList a b
