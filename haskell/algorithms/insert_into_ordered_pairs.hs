import Data.List

{-
 Amazon question:
 List of ordered pairs
 pairs stand for ranges - (lb, ub)
 Insert a new pair s.t. it remains ordered
-}

-- Return greater of two numbers
greater a b = (if (a>b) then a else b)

-- Used for an index, So if it's -ve, set it to 0
convert_to_natural x 
	| x < 0 = 0
	| otherwise = x

-- Merge two pairs if possible, Leave alone if disjoint
merge_ordered_pairs :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
merge_ordered_pairs pair1 pair2 
	| (fst pair2 < snd pair1) = [ ( (fst pair1), (greater (snd pair1) (snd pair2)) ) ]	
	| (fst pair2 == snd pair1 + 1) = [ ( (fst pair1), (snd pair2) ) ]
	| otherwise = [pair1, pair2]


-- Insert the new pair into an appropriate position in the list
insert_pair_into_list :: [(Int,Int)] -> (Int,Int) -> (Int,[(Int,Int)])
insert_pair_into_list lst new_pair = 
	let (left, right) = partition (\x -> (fst x < fst new_pair)) lst
	in
	(length(left), left ++ [new_pair] ++ right)


-- Reorder once a new pair has been inserted as long as u can keep merging
reorder_ordered_pairs :: [(Int,Int)] -> [(Int,Int)]
reorder_ordered_pairs [] = []
reorder_ordered_pairs (x:[]) = [x]
reorder_ordered_pairs (x:y:[]) = merge_ordered_pairs x y
reorder_ordered_pairs (x:y:lst) =
	if length (new_lst) == 1
		then reorder_ordered_pairs (new_lst ++ lst)
		else x:y:lst
	where new_lst = (merge_ordered_pairs x y)


-- Finally, the starter function to insert the pair in an "orderly" fashion
-- Inserts the pair first and then calls reorder on the 
-- right part of the list that needs re-ordering a.k.a merging
insert_into_ordered_pairs :: [(Int,Int)] -> (Int, Int) -> [(Int,Int)]
insert_into_ordered_pairs lst new_pair = 
	left_ordered_list ++ reorder_ordered_pairs right_unordered_list
	where
	(part_len, inserted_list) = insert_pair_into_list lst new_pair
	left_ordered_list = take (convert_to_natural part_len-1) inserted_list
	right_unordered_list = drop (convert_to_natural part_len-1) inserted_list

