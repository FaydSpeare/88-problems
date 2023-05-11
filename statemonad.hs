import Control.Monad.State -- first, import the state monad

type Stack = [Int]

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  

push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)


stackManip :: State Stack Int  
stackManip = do  
    push 3  
    a <- pop  
    pop

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

minusTree :: Int -> State (Tree Int) Int
minusTree n = state $ \(Branch x l r) -> (x-n, Branch (x-n) l r)

-- Playing around with the State monad but this is pretty senseless
minusTreeRec :: Int -> State (Tree Int) Int
minusTreeRec n = state myFunc
    where myFunc Empty = (0, Empty)
          myFunc (Branch x l r) = (x-n, Branch (x-n) left right)
              where (_, left) = runState (minusTreeRec n) l
                    (_, right) = runState (minusTreeRec n) r

