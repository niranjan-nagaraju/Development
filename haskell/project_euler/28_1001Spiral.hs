import IO

sum_spiralDiagonals n = 
	sum_spiralDiagonals' n 1 [1,1,1,1] [2,4,6,8]
	where
	sum_spiralDiagonals' :: Int -> Int -> [Int] -> [Int] -> Int
	sum_spiralDiagonals' n sumDiags diags next_diags = 
		if (diags !! 3) == n
			then sumDiags
			else
				sum_spiralDiagonals' n curr_sum new_diags new_next_diags
				where
				new_diags = zipWith (+) diags next_diags
				curr_sum = sumDiags + sum (new_diags)
				new_next_diags = map (+8) next_diags

main = do
	print $ sum_spiralDiagonals(1001*1001)
	{- 669171001 -}
