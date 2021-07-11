{- Flatten a nested list structure.
 -
 - Transform a list, possibly holding lists as elements into a `flat' list
 - by replacing each list with its elements (recursively).
 -
 - We have to define a new data type, because lists in Haskell are homogeneous.
 -
 -  data NestedList a = Elem a | List [NestedList a]
 -  λ> flatten (Elem 5)
 -  [5]
 -  λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
 -  [1,2,3,4,5]
 -  λ> flatten (List [])
 -  []
 -}

-- Solutions shamelessly lifted from the wiki page. :)

import Control.Exception( assert )

data NestedList a = Elem a | List [NestedList a] deriving Show
 
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x -- concatMap applies "flatten" to every element in the list and concatenates

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List []) = []
flatten' (List(x:xs)) = flatten' x ++ flatten' (List xs)

main = do
	putStr $ assert ( (flatten (List []::NestedList Int)) == [] ) ""
	putStr $ assert ( (flatten (Elem 5)) == [5] ) ""
	putStr $ assert ( (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])) == [1,2,3,4,5] ) ""
	putStr $ assert ( (flatten (List [List [List [Elem 1]]])) == [1] ) ""

	putStr $ assert ( (flatten' (List []::NestedList Int)) == [] ) ""
	putStr $ assert ( (flatten' (Elem 5)) == [5] ) ""
	putStr $ assert ( (flatten' (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])) == [1,2,3,4,5] ) ""
	putStr $ assert ( (flatten (List [List [List [Elem 1]]])) == [1] ) ""

