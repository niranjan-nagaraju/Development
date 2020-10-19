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
Solution:
	1. Sort A, B in decreasing order.
	2. The maximum sum = A[0]+B[0]
		2.1 The next maximum sum would be either A[1]+B[0] or A[0]+B[1], followed by
			A[1]+B[0] -> {A[1]+B[1], A[2]+B[1]}, A[0]+B[1] -> {A[1]+B[1], A[0]+B[2]}
			=> A[1]+B[1] is added twice in the above scenario.
			=> Use a set to ensure duplicate entries don't get added to the max-heap.
	3. Store (sum, i, j) into the max-heap
	4. Extract max 'N' times
		4.1 entry <- sum, i, j
		4.2 Add sum to the list, 
			Add A[i]+B[j+1] and A[i+1]+B[j] to the max-heap unless they are already part of the max-heap.
'''

from maxheap import MaxHeap
class Solution:
	def findMaxPairSums(self, A, B):
		if not A:
			return []

		A.sort(reverse=True)
		B.sort(reverse=True)

		n = len(A)
		heap = MaxHeap(n)
	
		heap.add((A[0]+B[0], 0, 0))
		lookup = set([(0,0)])
		
		n_max_pair_sums = []
		while heap:
			x,i,j = heap.extractMax()
			n_max_pair_sums.append(x)

			if len(n_max_pair_sums) == n:
				break

			if (i+1,j) not in lookup:
				lookup.add((i+1,j))
				heap.add((A[i+1]+B[j], i+1, j))
			if (i,j+1) not in lookup:
				lookup.add((i,j+1))
				heap.add((A[i]+B[j+1], i, j+1))

		return n_max_pair_sums


if __name__ == '__main__':
	s = Solution()
	assert s.findMaxPairSums([1,2], [3,4]) == [6,5]
	assert s.findMaxPairSums([1,4,2,3], [2,5,1,6]) == [10,9,9,8]

