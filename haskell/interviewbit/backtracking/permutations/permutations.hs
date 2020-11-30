{-
https://www.interviewbit.com/problems/permutations/

Permutations

Given a collection of numbers, return all possible permutations.

Example:

	[1,2,3] will have the following permutations:

	[1,2,3]
	[1,3,2]
	[2,1,3] 
	[2,3,1] 
	[3,1,2] 
	[3,2,1]

NOTE
No two entries in the permutation sequence should be the same.
For the purpose of this problem, assume that all the numbers in the collection are unique.
-}


{-
Solution Outline:
	A simple permutation-generator starts with only the first element,
	  Then at level 2, Makes 2 copies, Inserts second element at indices [0,1]
	  At level 3, Makes 3 copies of the previous level permutations, Inserts third element at indices [0,1,2] for each copy

	e.g.,
	A: [1, 2, 3]
	l0: []
	l1: [1]
	l2: [1] [1] -> [1,2], [2,1]
	l3: [1,2], [2,1] * 3 -> [1,2], [1,2], [1,2], [2,1], [2,1], [2,1]
		-> [1,2,3], [1,3,2], [3,1,2], [2,1,3], [2,3,1], [3,2,1]

	For a backtracking algorithm, Do a DFS traversal, at each (level, i), Add A[level] at index i and backtrack.
	At level == length(A), add current permutation to results list.

  A: [x, y, z]

                                         f([], x, 0):
                    /                                               \
                 f([x], y, 0)                                     f([x], y, 1)
            /          |         \                           /          |        \
f([y,x], z, 0)  f([y,x], z, 1)  f([y,x], z, 2)   f([x,y], z, 0)  f([x,y], z, 1)  f([x,y], z, 2)
  \               \               \                \               \               \    
 [z,y,x]          [y,z,x]         [y,x,z]          [z,x,y]         [x,z,y]         [x,y,z]

-}

import Data.List (sort)
import Control.Exception (assert)

-- Makes `n+1` copies of `a[]`
-- Slices each copy and inserts `x` into positions
-- 0..n respectively (n: length of a)
insert_slices :: [t] -> t -> [[t]]
insert_slices a x = map (insert_at a x) [0..length a]
	where insert_at lst x idx = (take idx lst) ++ [x] ++ (drop idx lst)


permute' :: [t] -> [t] -> Int -> [[t]]
permute' a prefix level
	| (length prefix == length a) = [prefix]
	| otherwise = foldr (\p acc -> acc ++ (permute' a p (level+1))) [] slices
	where slices = insert_slices prefix (a !! level)


permutations :: Ord t => [t] -> [[t]]
permutations a = sort $ permute' a [] 0


main = do
	-- putStr $ assert ( permutations [] == [[]] ) ""
	putStr $ assert ( (insert_slices [1,2,3] 4) == [[4,1,2,3], [1,4,2,3], [1,2,4,3], [1,2,3,4]] ) ""
	putStr $ assert ( (permutations [1]) == [[1]] ) ""
	putStr $ assert ( (permutations [1,2]) == [[1,2], [2,1]] ) ""
	putStr $ assert ( (permutations [1,2,3]) == [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]] ) ""
	putStr $ assert ( (permutations "abc") == ["abc","acb","bac","bca","cab","cba"] ) ""
	putStrLn $ "Testcases passed!"

