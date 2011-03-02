{- Flatten a nested list structure.
 -
 - Transform a list, possibly holding lists as elements into a `flat' list
 - by replacing each list with its elements (recursively).
 -}

-- Solution shamelessly lifted from page. :)

data NestedList a = Elem a | List [NestedList a]
 
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x -- concatMap applies "flatten" to every element in the list and concatenates

main = do
	print $ flatten (Elem 5)
	print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
	-- print $ flatten (List [])
