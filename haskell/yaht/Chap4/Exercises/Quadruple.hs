module Quadruple
	where

data Quadruple a b = Quadruple a a b b

firstTwo :: Quadruple a b -> [a]
firstTwo (Quadruple w x _ _) = [w,x]

lastTwo :: Quadruple a b -> [b]
lastTwo (Quadruple _ _ y z) = [y,z]
