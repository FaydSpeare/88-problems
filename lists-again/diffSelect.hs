import System.Random

randGet :: Int -> [Int] -> IO [Int]
randGet 0 _ = return []
randGet n xs = do
    newStdGen
    gen <- getStdGen
    let i = fst $ randomR (0, length xs - 1) gen
    let rest = take i xs ++ drop (i+1) xs
    ys <- randGet (n-1) rest
    return ((xs !! i):ys)

diffSelect :: Int -> Int -> IO [Int]
diffSelect n b = do
    result <- randGet n [1..b]
    return result
