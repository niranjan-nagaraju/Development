'''
Fill an array of size 2*n with each of numbers 1-n such that each number is repeated twice, but every number, i, has to be separated exactly 'i' slots between the two occurences.
eg, the two occurrences of 3 has to be separated  by  3 slots, 
Return all such arrangements for a specified n, or an empty list, [],  if such an arrangement does not exist
    n = 2: {not possible} => []
	n = 3: [[2, 3, 1, 2, 1, 3], [3, 1, 2, 1, 3, 2]]
	n = 4: [[2, 3, 4, 2, 1, 3, 1, 4], [4, 1, 3, 1, 2, 4, 3, 2]]
'''

'''
Solution Outline:
	Use backtracking to find slots for the next n>=i>=1, beginning with n at index (0, 2), then try to fit in n-1 recursively, 
	followed by n-2, .. 1
	then advance n to slot 1, etc, and repeat
	until all such arrangements are found.

	NOTE: Presumably, going wider at first (ie filling n..1) is better than conservative (1..n) as there will be fewer and fewer valid slots
		  and invalid arrangements can be discarded earlier. [citation needed!]

Example:
	n=3

	Array: _ _ _ _ _ _
	
	i:0
	  Array: 3 _ _ _ 3 _
	    => Fill 2, starting at index 1
			Array: 3 2 _ _ 3 _  (False)
		=> Fill 2, starting at index 2
			Array: 3 _ 2 _ 3 2
			=> Fill 1, starting at index 1
			Array: 3 1 2 1 3 2 [Ding!] ==> [3,1,2,1,3,2]
			=> Fill 1 starting at index 3 
				Array: 3 _ 2 1 3 2 [False]
		=> Fill 2, starting at index 3 (3+2+1 == 6 > 5)
			Array: 3 _ _ 2 3 _
		=> Fill 2, starting at index 5 (5+2+1 == 8 > 5)
			Array: 3 _ _ _ 3 2

	i:1
	  Array: _ 3 _ _ _ 3
	    => Fill 2, starting at index 0
			Array: 2 3 _ 2 _ 3
			=> Fill 1, starting at index 2
			Array: 2 3 1 2 1 3 [Ding!] ==> [2,3,1,2,1,3]
			=> Fill 1, starting at index 4 (4+1+1 == 6 > 5)
			Array: 2 3 _ 2 1 3
	    => Fill 2, starting at index 2
			Array: _ 3 2 _ _ 3
	    => Fill 2, starting at index 3 (3+2+1 == 6 > 5)
			Array: _ 3 _ 2 _ 3
	    => Fill 2, starting at index 4 (4+2+1 == 7 > 5)
			Array: _ 3 _ _ 2 3

	i:2
	  Array: _ _ 3 _ _ _ (3+2+1 == 6 > 5)
'''


def arrange(n):
	# Fill x in the first available slot,i, and i+x+1
	# then try to fill x+1, x+2, ... all the way upto n
	# If any of the fill fails for x, move over to the next available slot to the right
	# and restart filling all higher numbers
	def fill(arr, x):
		if x == 0:
			arrangements.append(arr[:])

		for i in xrange(2*n):
			if arr[i] == None and (i+x+1 < 2*n) and arr[i+x+1] == None:
				arr[i] = arr[i+x+1] = x
				fill(arr, x-1)

				# Backtrack
				arr[i] = arr[i+x+1] = None

	arrangements = []
	fill([None]*2*n, n)
	return arrangements


if __name__ == '__main__':
	assert arrange(1) == []
	assert arrange(2) == []
	assert arrange(3) == [[3, 1, 2, 1, 3, 2], [2, 3, 1, 2, 1, 3]]
	assert arrange(4) == [[4, 1, 3, 1, 2, 4, 3, 2], [2, 3, 4, 2, 1, 3, 1, 4]]
	assert arrange(5) == []
	assert arrange(6) == []
	assert len(arrange(7)) == 52
	assert arrange(10) == []

