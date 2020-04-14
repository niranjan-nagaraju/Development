'''
https://www.interviewbit.com/problems/rearrange-array/

Rearrange Array

Rearrange a given array so that Arr[i] becomes Arr[Arr[i]] with O(1) extra space.

Example:
	Input : [1, 0]
	Return : [0, 1]
	 Lets say N = size of the array. Then, following holds true :
	 All elements in the array are in the range [0, N-1]
	 N * N does not overflow for a signed integer
'''

'''
Solution Outline: O(1) memory, O(n) time
  If x = (a + bn),
    x%n = a
	x/n = b

  Use this to store both A[i], and A[A[i]] inside A[i]

  1. Add A[(A[i]%n)] * n to each A[i] {Since some of A[i] might already have b*n added, use A[i]%n to get its previous value}
  2. At the end of the first pass, each A[i] is of the form a+b*n
      where a is its previous value A[i], b is the value it's supposed to be replaced by (A[A[i]])
  3. Divide each A[i] by n so only b (out of a+b*n) remains	  
'''

class Solution:
	# @param A : list of integers
	# Modify the array A which is passed by reference. 
	# You do not need to return anything in this case. 
	def rearrange_array(self, A):
		n = len(A)
		for i in xrange(n):
			A[i] += (A[A[i]] % n) * n

		for i in xrange(n):
			A[i] /= n

		return A

if __name__ == '__main__':
	s = Solution()
	assert s.rearrange_array([1,0]) == [0,1]
	assert s.rearrange_array([0,1,2,3,4,5]) == [0,1,2,3,4,5]
	assert s.rearrange_array([4,2,0,1,3]) == [3,0,4,2,1]
	assert s.rearrange_array([0,0,1,0,0]) == [0,0,0,0,0]
	assert s.rearrange_array([4,0,2,1,3]) == [3,4,2,0,1]

