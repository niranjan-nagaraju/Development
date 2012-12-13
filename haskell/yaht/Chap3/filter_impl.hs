{- Haskell filter function implementation -}

filter' predicate [] = []
filter' predicate (x:xs) =
    if predicate x
        then x : filter' predicate xs
        else filter' predicate xs


{-
 - Trial runs
 -
 - *Main> filter' (==0) [0,1,2,3,4]
 - [0]
 - *Main> filter' (<4) [0,1,2,3,4]
 - [0,1,2,3]
 -} 
