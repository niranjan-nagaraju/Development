module Exponent
	where

power :: Integer -> Integer -> Integer
power a 1 = a
power a b = a * power a (b-1)
