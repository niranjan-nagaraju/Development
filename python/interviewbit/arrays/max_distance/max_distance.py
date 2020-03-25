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

class Solution:
	def max_distance(self, A):
		max_d = None
		for i in xrange(len(A)):
			for j in xrange(len(A)-1, i, -1):
				if A[i] <= A[j]:
					max_d = max(max_d, j-i)

		return 0 if max_d is None else max_d


if __name__ == '__main__':
	s = Solution()
	assert s.max_distance([3,5,4,2]) == 2
	assert s.max_distance([3,1]) == 0
	assert s.max_distance([2,1,4,3,6,5]) == 5
	assert s.max_distance([6,5,4,4,4,4,4,1]) == 4
