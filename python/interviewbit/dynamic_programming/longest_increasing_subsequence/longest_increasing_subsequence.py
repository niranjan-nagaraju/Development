#encoding: utf-8
'''
https://www.interviewbit.com/problems/longest-increasing-subsequence/

Longest Increasing Subsequence

Find the longest increasing subsequence of a given array of integers, A.

In other words, find a subsequence of array in which the subsequence’s elements are in strictly increasing order,
and in which the subsequence is as long as possible.
This subsequence is not necessarily contiguous, or unique.
In this case, we only care about the length of the longest increasing subsequence.


Input Format:
The first and the only argument is an integer array A.

Output Format:
Return an integer representing the length of the longest increasing subsequence.

Constraints:
1 <= length(A) <= 2500
1 <= A[i] <= 2000

Example :
Input 1:
    A = [1, 2, 1, 5]
Output 1:
    3
Explanation 1:
    The sequence : [1, 2, 5]

Input 2:
    A = [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]
Output 2:
    6
Explanation 2:
    The sequence : [0, 2, 6, 9, 13, 15] or [0, 4, 6, 9, 11, 15] or [0, 4, 6, 9, 13, 15]
'''

'''
Solution Outline:
	1. Let LIS_lens[i] be the length of the longest increasing sequence ending at A[i]
	2. LIS_lens[0..n-1] = 1
		Each element is an increasing-sequence on its own
	3. LIS_lens[i] = max{
							LIS_lens[j] if A[i] > A[j]
						}, 0 <= j < i
	4. Return the maximum value from LIS_lens[]

Sample run:
	A: [5, 1, 4, 3, 6, 2]
	LIS_lens: [1, 1, 1, 1, 1, 1]

	i: 1, x: 1
		x > 5? NO

	i: 2, x: 4
		x > 5? NO
		x > 1? YES
		LIS_lens[2] = max(LIS_lens[1], LIS_lens[2]+1) = max(1,1+1) = 2
		LIS_lens: [1, 1, 2, 1, 1, 1]

	i: 3, x: 3
		x > 5? NO
		x > 1? YES
		LIS_lens[3] = max(LIS_lens[1], LIS_lens[3]+1) = max(1,1+1) = 2
		LIS_lens: [1, 1, 2, 2, 1, 1]

	i: 4, x: 6
		x > 5? YES
		LIS_lens[4] = max(LIS_lens[0], LIS_lens[4]+1) = max(1,1+1) = 2
		LIS_lens: [1, 1, 2, 2, 2, 1]
		x > 1? YES
		LIS_lens[4] = max(LIS_lens[1], LIS_lens[4]+1) = max(1,1+1) = 2
		LIS_lens: [1, 1, 2, 2, 2, 1]
		x > 4? YES
		LIS_lens[4] = max(LIS_lens[2], LIS_lens[4]+1) = max(1,2+1) = 3
		LIS_lens: [1, 1, 2, 2, 3, 1]
		x > 3? YES
		LIS_lens[4] = max(LIS_lens[3], LIS_lens[4]+1) = max(1,2+1) = 3
		LIS_lens: [1, 1, 2, 2, 3, 1]

	i: 5, x: 2
		x > 5? NO
		x > 1? YES
		LIS_lens[5] = max(LIS_lens[0], LIS_lens[5]+1) = max(1,1+1) = 2
		LIS_lens: [1, 1, 2, 2, 3, 2]
		x > 4? NO
		x > 3? NO
		x > 6? NO

	max(LIS_lens): 3 == LIS length
'''
class Solution:
	# @param A : tuple of integers
	# @return an integer
	def lis(self, A):
		if not A:
			return 0
		lis_lens = [1] * len(A)
		for i in xrange(1, len(A)):
			for j in xrange(i):
				if A[i] > A[j]:
					lis_lens[i] = max(lis_lens[i], lis_lens[j]+1)

		return max(lis_lens)



if __name__ == "__main__":
	s = Solution()
	assert s.lis([5,1,4,3,6,2]) == 3
	assert s.lis([1, 2, 1, 5]) == 3
	assert s.lis([0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]) == 6

