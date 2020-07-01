#encoding: utf-8
'''
https://www.interviewbit.com/problems/length-of-longest-subsequence/

Length of Longest Subsequence

Problem Description
Given an 1D integer array A of length N, find the length of longest subsequence which is first increasing then decreasing.


Problem Constraints
0 <= N <= 3000
-10⁷ <= A[i] <= 10⁷

Input Format
The first and the only argument contains an integer array A.

Output Format
Return an integer representing the answer as described in the problem statement.

Example Input
Input 1:
	A = [1, 2, 1]
Input 2:
	A = [1, 11, 2, 10, 4, 5, 2, 1]

Example Output
Output 1:
	3
Output 2:
	6

Example Explanation
Explanation 1:
	[1, 2, 1] is the longest subsequence.
Explanation 2:
	[1 2 10 4 2 1] is the longest subsequence.
'''

'''
Solution Outline:
	1. Calculate LIS table where LIS[i] contains the longest increasing sequence _ending_ at A[i] {0<=i<n}
	2. Calculate LDS table where LDS[i] contains the longest decreasing sequence _starting_ at A[i] {0<=i<n}
	   2.1 LDS table by calculating LIS for reverse of A
	3. Longest Increasing-Decreasing subsequence would then be 
		max(
			{LIS[i]+LDS[i]-1} (0<=i<n)
		   )
		NOTE:
			A[i] is counted twice, once for the ending of LIS, and again for the start of the LDS, so reduce the max LIDS length by 1}
			LIS[-1]: Strictly increasing subsequence,followed by an empty decreasing sunsequence
			LDS[0]:  Strictly decreasing subsequence, following an empty increasing susequence

To calculate LIS DP table:
	0. Initialize LIS[0..n-1] = 1 {Each A[i] is an increasing subsequence on its own}
	1. For each i, 1 < i < n,
		 Calculate LIS[i] = max(LIS[i], LIS[j]+1, j: 0..i-1, if A[i] > A[j])

Calculate LDS DP table by calculating LIS for A in reverse.
	0. Initialize LDS[0..n-1] = 1 {Each A[i] is a decreasing subsequence on its own}
	1. For each i, n > i > 1,
		  Calculate LDS[i] = max(LDS[i], LDS[j]+1, j:n-1..i+1, if A[i] > A[j])

Sample run:
	A = [1, 11, 2, 10, 4, 5, 2, 1]
	     0   1  2   3  4  5  6  7

	LIS table:
	         0  1  2  3  4  5  6  7
	   LIS: [1, 1, 1, 1, 1, 1, 1, 1]
	   i: 1, A[i] = 11
	   LIS: [1, 2, 1, 1, 1, 1, 1, 1]
	   i: 2, A[i] = 2
	   LIS: [1, 2, 2, 1, 1, 1, 1, 1]
	   i: 3, A[i] = 10
	   LIS: [1, 2, 2, 3, 1, 1, 1, 1]
	   i: 4, A[i] = 4
	   LIS: [1, 2, 2, 3, 3, 1, 1, 1]
	   i: 5, A[i] = 5
	   LIS: [1, 2, 2, 3, 3, 4, 1, 1]
	   i: 6, A[i] = 2
	   LIS: [1, 2, 2, 3, 3, 4, 2, 1]
	   i: 7, A[i] = 1
	   LIS: [1, 2, 2, 3, 3, 4, 2, 1]

	LDS table:
	        -8 -7 -6 -5 -4 -3 -2 -1
	   LDS: [1, 1, 1, 1, 1, 1, 1, 1]
	   i: -2, A[i]: 2
	   LDS: [1, 1, 1, 1, 1, 1, 2, 1]
	   i: -3, A[i]: 5
	   LDS: [1, 1, 1, 1, 1, 3, 2, 1]
	   i: -4, A[i]: 4
	   LDS: [1, 1, 1, 1, 3, 3, 2, 1]
	   i: -5, A[i]: 10
	   LDS: [1, 1, 1, 4, 3, 3, 2, 1]
	   i: -6, A[i]: 2
	   LDS: [1, 1, 2, 4, 3, 3, 2, 1]
	   i: -7, A[i]: 11
	   LDS: [1, 5, 2, 4, 3, 3, 2, 1]
	   i: -8, A[i]: 1
	   LDS: [1, 5, 2, 4, 3, 3, 2, 1]

	LIS: 
	   [1, 2, 2, 3, 3, 4, 2, 1]
	LDS:
	   [1, 5, 2, 4, 3, 3, 2, 1]

	max(LIS[i]+LDS[i]-1) == 6
'''
class Solution:
	# Calculate LIS DP table
	# LIS[i]: longest increasing subsequence ending at A[i]
	def calculate_lis(self, A):
		lis = [1]*len(A)
		for i in xrange(1, len(A)):
			for j in xrange(i):
				if A[i] > A[j]:
					lis[i] = max(lis[i], lis[j]+1)
		return lis

	# Calculate LDS DP table
	# LDS[i]: longest decreasing subsequence starting at A[i]
	def calculate_lds(self, A):
		lds = [1]*len(A)
		for i in xrange(len(A)-1, -1, -1):
			for j in xrange(len(A)-1, i, -1):
				if A[i] > A[j]:
					lds[i] = max(lds[i], lds[j]+1)
		return lds


	# Find max LIDS length
	def find_lids_len(self, A):
		if not A:
			return 0

		lis = self.calculate_lis(A)
		lds = self.calculate_lds(A)

		max_lids_len = 1
		for i in xrange(len(A)):
			max_lids_len = max(max_lids_len, lis[i]+lds[i]-1)

		return max_lids_len



if __name__ == '__main__':
	s = Solution()
	A = [1,1,1,1,1,1,1]
	assert s.calculate_lis(A) == [1]*len(A)
	assert s.calculate_lds(A) == [1]*len(A)
	assert s.find_lids_len(A) == 1

	assert s.calculate_lis([1, 11, 2, 10, 4, 5, 2, 1]) == [1, 2, 2, 3, 3, 4, 2, 1]
	assert s.calculate_lds([1, 11, 2, 10, 4, 5, 2, 1]) == [1, 5, 2, 4, 3, 3, 2, 1]
	assert s.find_lids_len([1, 11, 2, 10, 4, 5, 2, 1]) == 6

	assert s.calculate_lis([1, 2, 1]) == [1, 2, 1]
	assert s.calculate_lds([1, 2, 1]) == [1, 2, 1]
	assert s.find_lids_len([1, 2, 1]) == 3

	assert s.calculate_lis([1, 1, 2, 1, 1]) == [1, 1, 2, 1, 1]
	assert s.calculate_lds([1, 1, 2, 1, 1]) == [1, 1, 2, 1, 1]
	assert s.find_lids_len([1, 1, 2, 1, 1]) == 3

	A =  [8, 6, 3, 4, 2, 1]
	assert s.calculate_lis(A) == [1, 1, 1, 2, 1, 1]
	assert s.calculate_lds(A) == [5, 4, 3, 3, 2, 1]
	assert s.find_lids_len(A) == 5

	A = [1, 2, 3, 4, 4, 5, 5, 5, 5, 5]
	assert s.calculate_lis(A) == A
	assert s.calculate_lds(A) == [1]*len(A)
	assert s.find_lids_len(A) == 5

	A = [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]
	assert  s.calculate_lis(A) == [1, 2, 2, 3, 2, 3, 3, 4, 2, 4, 3, 5, 3, 5, 4, 6]
	assert  s.calculate_lds(A) == [1, 4, 3, 5, 2, 4, 3, 4, 1, 3, 2, 3, 1, 2, 1, 1]
	assert s.find_lids_len(A) == 7

