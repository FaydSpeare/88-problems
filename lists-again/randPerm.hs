import System.Random

randPerm :: [a] -> IO [a]
randPerm [] = return []
randPerm [x] = return [x]
randPerm xs = do
    newStdGen
    gen <- getStdGen
    let i = fst $ randomR (0, length xs - 1) gen
    rest <- randPerm (take i xs ++ drop (i+1) xs) 
    return $ (xs !! i) : rest 
