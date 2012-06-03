module LCG (lcg)
	where

{- Linear Congruential Generator --}

{- 
 - X{i+1} = ( a. X{i} + b ) % (m + 1)
 -}


-- lcg :: Int->Int->Int->Int->[Int]
lcg a b m x =
	x : lcg a b m xi
	where
		xi = (a * x + b) `mod` m


