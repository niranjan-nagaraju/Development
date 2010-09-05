findEleMatchPredicate :: (a -> Bool) -> [a] -> Maybe a
findEleMatchPredicate p [] = Nothing
findEleMatchPredicate p (x:xs) = 
	if p x then Just x
	else findEleMatchPredicate p xs
