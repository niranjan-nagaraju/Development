{- Define Tuple class for exercise 4.6 and 4.7 -}

module Tuple
	where

data Tuple a b c d = One a
				   | Two a b
				   | Three a b c
				   | Four a b c d


{- Create four 'Tuple' objects with 1,2,3,4 members -}
one = One 1
two = Two 2 "2"
three = Three 3 "3" '3'
four = Four 4 "4" '4' 4.0

