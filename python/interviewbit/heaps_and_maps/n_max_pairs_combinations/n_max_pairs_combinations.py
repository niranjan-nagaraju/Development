'''
https://www.interviewbit.com/problems/n-max-pair-combinations/

N max pair combinations

Given two arrays A & B of size N each.
Find the maximum N elements from the sum combinations (Ai + Bj) formed from elements in array A and B.

For example if A = [1,2], B = [3,4], then possible pair sums can be 1+3 = 4 , 1+4=5 , 2+3=5 , 2+4=6
and maximum 2 elements are 6, 5

Example:

	N = 4
	a[]={1,4,2,3}
	b[]={2,5,1,6}

Maximum 4 elements of combinations sum are
	10   (4+6), 
	9    (3+6),
	9    (4+5),
	8    (2+6)
'''


'''
Solution: (Brute-force solution)
	1. Generate N*N pair-sums.
	2. Store them in a max-heap
	3. Extract max-heap 'N' times, and return the results.
'''

from maxheap import MaxHeap
class Solution:
	def findMaxPairSums(self, A, B):
		if not A:
			return []

		heap = MaxHeap(len(A)*len(A))

		for x in A:
			for y in B:
				heap.add(x+y)

		return [heap.extractMax() for _ in xrange(len(A))]



if __name__ == '__main__':
	s = Solution()
	assert s.findMaxPairSums([1,2], [3,4]) == [6,5]
	assert s.findMaxPairSums([1,4,2,3], [2,5,1,6]) == [10,9,9,8]

