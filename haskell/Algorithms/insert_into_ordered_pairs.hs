
greater a b = (if (a>b) then a else b)


-- Merge two pairs if possible, Leave alone if disjoint
-- merge_ordered_pairs :: (a,a)->(a,a)->[(a,a)]
merge_ordered_pairs pair1 pair2 =
	if fst(pair2) < snd(pair1)
		then [ ( (fst pair1), (greater (snd pair1) (snd pair2)) ) ]
		else [pair1, pair2]
