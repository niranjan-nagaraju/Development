{- 
 - a * b = { a if b is 1;
 -			 a + a * (b-1), for all b
 -}

mult :: Integer -> Integer -> Integer
mult a 1 = a
mult a b = a + mult a (b-1)
