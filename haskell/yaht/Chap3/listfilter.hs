{- listfilter:
 -	Arg1: Function {- That takes one element from list and returns Bool -}
 -	Arg2: List on which filter needs to be applied
 -	Return: Filtered List which matches the predicate function
 -}

listfilter :: (a->Bool) -> [a] -> [a]
listfilter p [] = []
listfilter p (x:xs) = 
	if p x	-- Prepend x
		then x : listfilter p xs
		else listfilter p xs

{-
 - Test Run:
 - *Main> listfilter id [False, True, False, True, True]
 - [True,True,True]
 -}
