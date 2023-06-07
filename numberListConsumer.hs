import Control.Applicative (Alternative, empty, many, some, (<|>))


data Ski
 = I
 | K
 | S
 | App Ski Ski
 deriving (Show, Eq)

eval :: Ski -> Ski

eval (App I x) = eval x

eval (App K x) = App K $ eval x
eval (App (App K x) y) = eval x

eval (App S x) = App S $ eval x
eval (App (App S x) y) = App (App S (eval x)) (eval y)
eval (App (App (App S x) y) z) = eval $ App (App x z) (App y z)

eval (App f x) = eval (App (eval f) x)
eval x = x


data Sumer a = Sumer { consume :: [Int] -> Maybe (a, [Int]) }

instance Functor Sumer where
    fmap f (Sumer s)= Sumer $ \ints -> case (s ints) of
        (Just (a, rest)) -> Just (f a, rest)
        Nothing -> Nothing

instance Applicative Sumer where
    pure v = Sumer $ \ints -> Just (v, ints)
    sf <*> s = Sumer $ \ints -> case (consume sf ints) of
        (Just (f, rest)) -> case (consume s rest) of
            (Just (v, rest')) -> Just (f v, rest')
            Nothing -> Nothing
        Nothing -> Nothing

instance Alternative Sumer where
    empty = Sumer $ const Nothing
    s1 <|> s2 = Sumer $ \ints -> case (consume s1 ints) of
        Just x -> Just x
        Nothing -> case (consume s2 ints) of
            Just y -> Just y
            Nothing -> Nothing

instance Monad Sumer where
    return = pure
    s >>= f = Sumer $ \ints -> case (consume s ints) of
        (Just (v, rest)) -> consume (f v) rest
        Nothing -> Nothing

num :: Int -> Sumer Int
num n = Sumer $ \ints -> case ints of
    [] -> Nothing
    (x:xs) -> if x == n then Just (n, xs) else Nothing

sum':: Int -> Sumer Int
sum' n = Sumer $ \ints -> case ints of
    [] -> Just (n, [])
    (x:xs) -> Just (n+x, xs)

mul':: Int -> Sumer Int
mul' n = Sumer $ \ints -> case ints of
    [] -> Just (n, [])
    (x:xs) -> Just (n*x, xs)

sumList :: [Int] -> Sumer Int
sumList ns = Sumer $ \ints -> Just (foldr (+) 0 ns, ints)

seqA :: Sumer [Int]
seqA = do
    x <- num 1
    y <- num 2 
    z <- num 3
    return [x, y, z]

seqB :: Sumer [Int]
seqB = do
    x <- num 1
    y <- num 1
    z <- num 2
    return [x, y, z]

eitherSeq :: Sumer [Int]
eitherSeq = seqA <|> seqB
