module Factorials
	where

{- Recursive factorial -}
factorialR :: Integer -> Integer
factorialR 0 = 1
factorialR x = 
	if x < 0
		then 1
		else x * factorialR (x-1)


{- Linear: Multiply all numbers from 1 to n -}
factorial :: Integer -> Integer
factorial 0 = 1
factorial x = 
	if x < 0
		then 1
		else product [1..x]
