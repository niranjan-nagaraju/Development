'''
https://www.interviewbit.com/problems/noble-integer/

Noble Integer
Given an integer array, find if an integer p exists in the array such that the number of integers greater than p in the array equals to p
If such an integer is found return 1 else return -1.
'''

'''
Solution Outline: O(n+max-min) time, O(n) memory
	1. Calculate min, max of A[]
	2. Setup a hash table to store frequencies of [min.. max], 
	    or [0..h], h = max-min+1, mapping a[i] -> table[a[i]-min]
	3. Scan A[i] and update frequency of A[i] in table
	4. Scan all slots in table if they are part of the  hash table (slots, s, whose key doesn't belong in table => (min+s) is part of A[])
	    while updating cumulative frequencies seen so far
		if A[i]'s frequency is f, and number of elements < A[i] == cf
		then,
		  number of integers greater than A[i], nge_i = n-1-(f-1)-cf
	4. Check if A[i] == nge_i
	5. update cf = cf + f for next non-empty slot in lookup table

Sample run 1:
	A: [2,3,2,1,5]
	n = 5

	min = 1, max = 5
	table = {}
	
	Update frequencies
	  i: 0, A[i] = 2, table[1] = 1
	  i: 1, A[i] = 3, table[2] = 1
	  i: 2, A[i] = 2, table[1] = 2
	  i: 3, A[i] = 1, table[0] = 1
	  i: 4, A[i] = 5, table[4] = 1
	  Table: [1, 2, 1, _, 1]

	Scan table to find noble integer
	  cf = 0
	  i: 1, idx = 0, f=table[idx] == 1
	    n-1-(f-1)-cf = 5-1-0-0 = 4
		cf = 1
	  i: 2, idx = 1, f=table[idx] = 2
	    n-1-(f-1)-cf = 5-1-(2-1)-1 = 5-1-1-1=2 == i
		return 1


Sample run 2:
	A: [4,6,1,1,3,6,3]
	n: 7

	min: 1, max: 6
	table: {}

	Update frequencies
	  i: 0, A[i] = 4, table[3] = 1
	  i: 1, A[i] = 6, table[5] = 1
	  i: 2, A[i] = 1, table[0] = 1
	  i: 3, A[i] = 1, table[0] = 2
	  i: 4, A[i] = 3, table[2] = 1
	  i: 5, A[i] = 6, table[5] = 2
	  i: 6, A[i] = 3, table[2] = 2 
	table: [2, _, 2, 1, _, 2]

	Scan table to find noble integer
	  cf = 0
	  i: 1, idx = 0, f=table[idx] == 2
	    n-1-(f-1)-cf = 7-1-1-0 = 5
		cf += 2 = 2
	  i: 2, idx = 1, f=table[1] =  0 --> skip  [2 is not in A]
	  i: 3, idx = 2, f=table[2] = 2
	    n-1-(f-1)-cf = 7-1-1-2 = 3 == i
		return 1
'''

from collections import defaultdict
class Solution:
	def is_there_noble_integer(self, A):
		n = len(A)
		minimum, maximum = A[0], A[0]
		for i in xrange(1, n):
			if A[i] < minimum:
				minimum = A[i]
			elif A[i] > maximum:
				maximum = A[i]

		table = defaultdict(lambda: 0)

		# update frequencies
		for x in A:
			table[(x-minimum)] += 1

		# Scan non-empty slots in lookup table
		cf = 0
		for i in xrange(0, maximum-minimum+1):
			f = table[i]
			if f == 0:
				continue
		
			nge_i = n-1-(f-1)-cf
			if nge_i == i+minimum:
				#print nge_i
				return 1

			cf += f

			
		return -1


if __name__ == '__main__':
	s = Solution()
	assert s.is_there_noble_integer([2,3,2,1,4]) == 1
	assert s.is_there_noble_integer([5,1,2,3,4]) == -1
	assert s.is_there_noble_integer([6,5,1,2,3,4]) == 1
	assert s.is_there_noble_integer([5,5,1,2,3,6]) == 1
	assert s.is_there_noble_integer([4,6,1,1,3,6,3]) == 1

