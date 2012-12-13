{- Recursive implementation of Haskell 'map' -}

listmap :: (a -> b) -> [a] -> [b]
listmap f [] = []
listmap f (x:xs) = (f x) : listmap f xs
