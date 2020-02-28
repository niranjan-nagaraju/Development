'''
https://www.interviewbit.com/problems/first-missing-integer/

First Missing Integer


Given an unsorted integer array, find the first missing positive integer.

Example:

Given [1,2,0] return 3,

[3,4,-1,1] return 2,

[-8, -7, -6] returns 1

Your algorithm should run in O(n) time and use constant space.
'''

'''
Solution Outline:
	Use an Array [1...n] as a lookup table instead of a hash-set
	Store A[x] when x is found, Ignore 0 and negative numbers since they dont need to be looked up.
	Re-scan to find if A[x] is present, 1 <= x <= n+1
	  Return the first absent entry
'''
class Solution:
    # @param A : list of integers
    # @return an integer
    def firstMissingPositive(self, A):
		n = len(A)
		lookup = [0] * (n+1)  # 1-indexed, 0 is unused

		for x in A:
			if x > 0 and x < (n+1):
				lookup[x] = 1

		# Look for missing number between [1..n]
		for i in xrange(1, n+1):
			if not lookup[i]:
				return i

		# All [1..n] are found in the list
		# => (n+1) is the first +ve integer missing
		return (n+1)


if __name__ == '__main__':
	s = Solution()
	assert s.firstMissingPositive([1,2,0]) == 3
	assert s.firstMissingPositive([3,4,-1,1]) == 2
	assert s.firstMissingPositive([-8, -7, -6]) == 1
	assert s.firstMissingPositive([1,2,3,4,5]) == 6
	assert s.firstMissingPositive([1,3,2,5,0,4]) == 6
	assert s.firstMissingPositive([1,5,2,4]) == 3
	assert s.firstMissingPositive([6,0,1,4,3,2]) == 5


