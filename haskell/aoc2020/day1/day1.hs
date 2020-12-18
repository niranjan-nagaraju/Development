import Control.Exception (assert)
import Data.List (sort)


{- 
 - Helper function to find a pair (a,b) s.t  a+b == target,
 - using the two pointer method.
 - and return a*b if they exist, else Nothing
 -}
find_pairs_with_sum' lst l r target 
	| (l >= r) = Nothing
	| (left+right == target) = Just (left*right)
	| (left+right < target) = find_pairs_with_sum' lst (l+1) r target
	| (left+right > target) = find_pairs_with_sum' lst l (r-1) target
	where
		left = (lst !! l)
		right = (lst !! r)


{-
 - Use helper function above to find a pair that adds upto target
 - and return its product
 - Sort the list before calling the helper function
 -}
find_pairs_with_sum [] _ = -1
find_pairs_with_sum lst target =
	case (find_pairs_with_sum' (sort lst)  0 ((length lst)-1) target) of
		Just x  -> x
		Nothing -> -1


{-
 - Find triplets that add upto a target sum, and return their product
 -}
find_triplets_with_sum' [] _ = -1
find_triplets_with_sum' (x:xs) target =
	case (find_pairs_with_sum xs (target-x)) of
		-1 -> find_triplets_with_sum' xs target
		y -> x*y


{-
 - Sort list
 - For each a in list, find a pair (b,c) to the right of a
 - s.t. a+b+c == target
 - return a*b*c
 -}
find_triplets_with_sum lst target = find_triplets_with_sum' (sort lst) target


{-
 - read lines from stdin until EOF, convert them to integers
 - and accumulate in a list
 -}
readLinesToList :: IO [Int]
readLinesToList = do
	s <- getContents
	return (map read (lines s) :: [Int])


{-
 - Basic testcases for Part 1
 -}
run_tests1 = do
    putStr $ (assert $ (find_pairs_with_sum [1721,979,366,299,675,1456] 2020) == 514579) ""
    putStr $ (assert $ (find_pairs_with_sum [1,5,8,9,2,3,7] 15) == 56) "" -- 8*7
    putStr $ (assert $ (find_pairs_with_sum [1,5,8,9,2,3,7] 14) == 45) "" -- 5*9
    putStr $ (assert $ (find_pairs_with_sum [1,5,8,9,2,3,7] 18) == -1) ""


{-
 - Basic testcases for Part 2
 -}
run_tests2 = do
    putStr $ (assert $ (find_triplets_with_sum [1721,979,366,299,675,1456] 2020) == 241861950) ""
    putStr $ (assert $ (find_triplets_with_sum [1,5,8,9,3,7] 15) == 45) "" -- 1*5*9
    putStr $ (assert $ (find_triplets_with_sum [1,5,8,9,3,7] 22) == 360) "" -- 5*8*9
    putStr $ (assert $ (find_triplets_with_sum [1,5,8,9,3,7] 23) == -1) ""


main = do
	run_tests1
	run_tests2
	lst <- readLinesToList

	-- part 1
	print $ find_pairs_with_sum lst 2020
	-- part 2
	print $ find_triplets_with_sum lst 2020

