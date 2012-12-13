-- We’ve seen how to calculate sums and products using folding functions.
-- Given that the function max returns the maximum of two numbers, write a function using a fold that will return the maximum value in a list (and zero if the list is empty).
-- So, when applied to [5,10,2,8,1] it will return 10. Assume that the values in the list are always ≥ 0. Explain to yourself why it works.

import Control.Monad

maxInList :: [Integer] -> Integer
maxInList mylist = 
	foldl max tmpMax mylist -- Repeatedly compare tmpMax and mylist[i]
	where tmpMax = 0 -- return 0 on empty list
    -- (head mylist)  -- tmpMax = mylist[0]

main = do
	n <- readLn
	inList <- forM [1..n] (\a -> readLn)
	print inList
	print $ maxInList inList
