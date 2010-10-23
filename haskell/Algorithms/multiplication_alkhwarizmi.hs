{-
 To multiply two numbers a and b
 Start with a and b
 Divide a by 2, multipy b by 2
 Keep going until a becomes 1
 Add all b values whose corresponding a values aren't even

 11	13 
 5	26 
 2	52	(strike out) 
 1 104
 ------
 143	(answer)
 ------
-}

multiply a b =
	multiply' a b 0
	where
	multiply' 1 b prod = (prod + b)
	multiply' a b prod = 
		if (a `mod` 2 == 1)
			then multiply' (a `div` 2) (b * 2) (prod + b)
			else multiply' (a `div` 2) (b * 2) prod
