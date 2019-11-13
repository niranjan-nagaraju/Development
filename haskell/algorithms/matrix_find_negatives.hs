{-
http://www.geeksforgeeks.org/count-negative-numbers-in-a-column-wise-row-wise-sorted-matrix/

Find number of negatives in the matrix sorted row-wise and column-wise

Input:  
[-3, -2, -1,  1]
[-2,  2,  3,  4]
[4,   5,  7,  8]
Output : 4


Solution: (recursive version)
	Start with the full matrix, rightmost column, row 0,
	if the number at that index, is -ve, 
		=> everything to the left is -ve
		Add column number to count, recursively work on the rest of the matrix sans current row, and every row above
	else, 
		skip current column, recursively work on the rest of the matrix sans current column, and every column to the right
-}


import Control.Exception (assert)


-- find_negatives [[matrix]] rows columns : returns count of negative numbers in the matrix
find_negatives :: [[Int]] -> Int -> Int -> Int
find_negatives matrix rows columns 
	| (rows == 0)    = 0 -- if rows == 0, return 0
	| (columns == 0) = 0 -- if columns == 0, return 0
	| ((last (head matrix)) < 0) = columns + (find_negatives (tail matrix) (rows-1) columns)
		{-
			Found a -ve element
			Add column number to count
			skip current row

			e.g.
			[
			  [-1, -2, -3, -4],
			  [w, x, y, z],
			  . . .
			]
			=> 4 + find_negatives(
					[
					  [w, x, y, z],
					  . . .
					])

			ghci: (to skip the first row in a matrix)
				Prelude> m
				[[1,2,3],[4,5,6],[7,8,9]]

				Prelude> tail m
				[[4,5,6],[7,8,9]]
		-}
	| otherwise = find_negatives (map init  matrix) rows (columns-1)
		{-
			Found a non-negative element
			skip current column

			e.g.
			[
			  [1, 2, 3, 4],
			  [w, x, y, z],
			  . . .
			  [a, b, c, d]
			]
			=> find_negatives(
					[
					  [1, 2, 3],
					  [w, x, y],
					  . . .
					  [a, b, c]
					])

			ghci: (to skip last column in a matrix)
				Prelude> m
				[[1,2,3],[4,5,6],[7,8,9]]

				Prelude> map init m
				[[1,2],[4,5],[7,8]]
		-}



		
-- Testcases
testcases :: [Char]
testcases = 
	assert ((find_negatives [[-1,-2,3],[4,5,6]] 2 3) == 2) "" ++

	assert ((find_negatives 
			[
			  [-3, -2, -1,  1],
			  [-2,  2,  3,  4],
			  [4,   5,  7,  8]
			] 3 4) == 4) "" ++

	assert ((find_negatives [[-1, -2, -3, -4]] 1 4) == 4) ""  ++

	assert ((find_negatives
			[
				[-7, -6, -5, -4],
				[-6, -5, -4, -3],
				[-5, -4, -3, -2],
				[-4, -3, -2, -1]
			] 4 4) == 16) "" ++ 
	
		assert ((find_negatives
		[
			[1, 2, 3, 4],
			[2, 3, 4, 5],
			[3, 4, 5, 6],
			[4, 5, 6, 7]
		] 4 4) == 0) "" ++

	assert ((find_negatives
		[
			[-4, 2, 3, 4],
			[-3, 3, 4, 5],
			[-2, 4, 5, 6],
			[-1, 5, 6, 7]
		] 4 4) == 4)  "" ++

	assert ((find_negatives
		[
			[1, 2, 3],
			[2, 3, 4],
			[4, 5, 6]
		] 3 3) == 0) ""



main :: IO()
main = do
	putStr $ testcases

