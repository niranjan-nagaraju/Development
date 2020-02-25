'''
Generate all subsets of a list  where none of the elements in the subset
are adjacent to each other in the original list

E.g.,
 A: [1,2,3,4,5]
 subsets:
   [1]
   [1,3]
   [1,3,5]
   [1,4]
   [1,5]
   [2]
   [2,4]
   [2,5]
   [3]
   [3,5]
   [4]
   [5]
'''

'''
Solution Outline:
	Recursively call subset_function() to handle subset of length x at depth x
	[1], is handled at first-level
	 [1,3] is handled at second
	   [1,3,5] at third and so on
	   Once we run out of index+2 elements to add,
	     Replace last element with index+1
	     so, after [1,3,5], Do [1,3,6], then [1,3,7] if A: [1,2,3,4,5,6,7] for eg
'''
def non_adjacent_subsets(a):
	subsets = []
	def non_adjacent_subsets_(subs):
		x = subs[-1]
		if x >= len(a):
			return

		subsets.append(map(lambda x: a[x], subs))

		# Add index+2 elements
		non_adjacent_subsets_(subs + [x+2])

		# Once we are done with index+2, replace last element in the subset with index+1
		# so we are still handling the same length subset at this level
		non_adjacent_subsets_(subs[:-1] + [x+1])

	non_adjacent_subsets_([0])
	return subsets


if __name__ == '__main__':
	assert non_adjacent_subsets(range(1,6)) == [
				[1],
				[1, 3],
				[1, 3, 5],
				[1, 4],
				[1, 5],
				[2],
				[2, 4],
				[2, 5],
				[3],
				[3, 5],
				[4],
				[5]
			]

	assert non_adjacent_subsets(range(1,10)) == [
				[1],
				[1, 3],
				[1, 3, 5],
				[1, 3, 5, 7],
				[1, 3, 5, 7, 9],
				[1, 3, 5, 8],
				[1, 3, 5, 9],
				[1, 3, 6],
				[1, 3, 6, 8],
				[1, 3, 6, 9],
				[1, 3, 7],
				[1, 3, 7, 9],
				[1, 3, 8],
				[1, 3, 9],
				[1, 4],
				[1, 4, 6],
				[1, 4, 6, 8],
				[1, 4, 6, 9],
				[1, 4, 7],
				[1, 4, 7, 9],
				[1, 4, 8],
				[1, 4, 9],
				[1, 5],
				[1, 5, 7],
				[1, 5, 7, 9],
				[1, 5, 8],
				[1, 5, 9],
				[1, 6],
				[1, 6, 8],
				[1, 6, 9],
				[1, 7],
				[1, 7, 9],
				[1, 8],
				[1, 9],
				[2],
				[2, 4],
				[2, 4, 6],
				[2, 4, 6, 8],
				[2, 4, 6, 9],
				[2, 4, 7],
				[2, 4, 7, 9],
				[2, 4, 8],
				[2, 4, 9],
				[2, 5],
				[2, 5, 7],
				[2, 5, 7, 9],
				[2, 5, 8],
				[2, 5, 9],
				[2, 6],
				[2, 6, 8],
				[2, 6, 9],
				[2, 7],
				[2, 7, 9],
				[2, 8],
				[2, 9],
				[3],
				[3, 5],
				[3, 5, 7],
				[3, 5, 7, 9],
				[3, 5, 8],
				[3, 5, 9],
				[3, 6],
				[3, 6, 8],
				[3, 6, 9],
				[3, 7],
				[3, 7, 9],
				[3, 8],
				[3, 9],
				[4],
				[4, 6],
				[4, 6, 8],
				[4, 6, 9],
				[4, 7],
				[4, 7, 9],
				[4, 8],
				[4, 9],
				[5],
				[5, 7],
				[5, 7, 9],
				[5, 8],
				[5, 9],
				[6],
				[6, 8],
				[6, 9],
				[7],
				[7, 9],
				[8],
				[9]
			]

