module Quadruple
	where

data Quadruple a b = Quadruple a a b b

firstTwo :: Quadruple a b -> [a]
firstTwo (Quadruple w x _ _) = [w,x]

lastTwo :: Quadruple a b -> [b]
lastTwo (Quadruple _ _ y z) = [y,z]


{-
 - Trial runs
 -
 -	*Quadruple> let a = Quadruple 1 5 '2' '3'
 -	*Quadruple> firstTwo a
 -	[1,5]
 -	*Quadruple> lastTwo a
 -	"23"
 -
 - Handle Different type errors?
 -  *Quadruple> let q' = Quadruple 1 'a' 2 'b'
 -
 -  <interactive>:1:25:
 -  No instance for (Num Char)
 -		arising from the literal `2' at <interactive>:1:25
 -  Possible fix: add an instance declaration for (Num Char)
 -  In the third argument of `Quadruple', namely `2'
 -  In the expression: Quadruple 1 'a' 2 'b'
 -                            In the definition of `q'': q' = Quadruple 1 'a' 2 'b'
 -                            *Quadruple> :t Quadruple
 -                            Quadruple :: a -> a -> b -> b -> Quadruple a b
 -}	
