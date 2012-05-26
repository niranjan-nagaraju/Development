import Data.List

{-
 Amazon question:
 List of ordered pairs
 pairs stand for ranges - (lb, ub)
 Insert a new pair s.t. it remains ordered
-}

greater a b = (if (a>b) then a else b)


-- Merge two pairs if possible, Leave alone if disjoint
merge_ordered_pairs :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
merge_ordered_pairs pair1 pair2 
	| (fst pair2 < snd pair1) = [ ( (fst pair1), (greater (snd pair1) (snd pair2)) ) ]	
	| (fst pair2 == snd pair1 + 1) = [ ( (fst pair1), (snd pair2) ) ]
	| otherwise = [pair1, pair2]


insert_into_ordered_pairs :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
insert_into_ordered_pairs lst new_pair = 
	let (left, right) = partition (\x -> (fst x < fst new_pair)) lst
	in
	left ++ [new_pair] ++ right
