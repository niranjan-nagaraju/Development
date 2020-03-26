'''
https://www.interviewbit.com/problems/max-distance/

Max Distance

Given an array A of integers, find the maximum of j - i subjected to the constraint of A[i] <= A[j].
If there is no solution possible, return -1.

Example :
	A : [3 5 4 2]
	Output : 2 
	for the pair (3, 4)
'''

'''
Solution Outline:
	B: [(i, A[i])] for i in 0..n
	sort B by A[i]
	Now we have a list of indices, where A[B[i]] <= A[B[i+1]]
	So given only this list of ordered indices, find a (j-i) that is maximum.
	[This is the same as the 'stock buy'  problem with a single transaction allowed]
	  Initially maximum is 0
	  i: B[0], j: B[0]
	  for each index in B[1:]:
		if index < i:
		   then i = index # this might be a better candidate for i

	    # Count distance to current index from a potential i
		# If its better than the previous maximum, Update i,j
	    if index-i > maximum:
		   maximum = index-i
		   j = index


Sample run 1:
	A:  [3,5,4,2]
	B:  [(3,0), (5,1), (4,2), (2,3)]
	B': [(2,3), (3,0), (4,2), (5,1)]
	B': [3, 0, 2, 1]  -- Only the indices

	Maximum: 0
	i: 3, j: 3

	index: 0
	index < i:
	  i = 0

	index: 2
	 index > i
	 index-i = 2-0 = 2 > maximum
	   maximum = 2
	  
	index: 1
	  index > i
	  index-i = 1-0 < maximum
	
  return 2


Sample run 2:
	A:  [2, 1, 4, 3, 6, 5]
	B:  [(2,0), (1,1), (4,2), (3,3), (6,4), (5,5)]
	B': [(1,1), (2,0), (3,3), (4,2), (5,5), (6,4)]
	B': [1, 0, 3, 2, 5, 4]

	Maximum: 0
	i: 1, j: 1

	index: 0
	  index < i
	    i = 0

	index: 3
	  index-i = 3 > maximum
	    maximum = 3

	index: 2
	  index-i = 2 < maximum

	index: 5
	  index-i = 5 > maximum
	    maximum = 5

	index: 4
	  index-i = 4 < maximum

  return 5
'''

class Solution:
	def max_distance(self, A):
		max_d = 0
		
		B = sorted([(A[i], i) for i in xrange(len(A))])
		i = j = B[0][1]
		for _,index in B[1:]:
			if index < i:
				i = index

			elif index-i > max_d:
				max_d = index-i
				j = index
		return max_d


if __name__ == '__main__':
	s = Solution()
	assert s.max_distance([3,5,4,2]) == 2
	assert s.max_distance([3,1]) == 0
	assert s.max_distance([2,1,4,3,6,5]) == 5
	assert s.max_distance([6,5,4,4,4,4,4,1]) == 4
