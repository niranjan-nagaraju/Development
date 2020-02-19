module BinarySearch (binary_search) where

binary_search' lst key low high
	| (low > high) = -1  -- Couldn't find key
	| (lst !! mid) == key = mid -- found key
	| (lst !! mid) < key = binary_search' lst key (mid+1) high -- key is to the right half
	| (lst !! mid) > key = binary_search' lst key low (mid-1) -- key is to the left half
	where
	mid = (low + high) `div` 2


binary_search lst key = binary_search' lst key 0 (length lst - 1)
