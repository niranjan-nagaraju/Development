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
Solution Outline:
	Brute force, O(n) memory
	Copy A[A[i]] into B[], return B
'''

class Solution:
	def rearrange_array(self, A):
		B = [None]*len(A)
		i = 0
		for x in A:
			B[i] = A[x]
			i += 1

		return B


if __name__ == '__main__':
	s = Solution()
	assert s.rearrange_array([1,0]) == [0,1]
	assert s.rearrange_array([0,1,2,3,4,5]) == [0,1,2,3,4,5]
	assert s.rearrange_array([4,2,0,1,3]) == [3,0,4,2,1]
	assert s.rearrange_array([0,0,1,0,0]) == [0,0,0,0,0]
	assert s.rearrange_array([4,0,2,1,3]) == [3,4,2,0,1]

