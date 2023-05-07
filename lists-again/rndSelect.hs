import System.Random

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
    gen <- getStdGen
    return $ map (xs !!) $ take n $ randomRs (0, length xs - 1) gen
