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
class Solution:
    # @param A : list of integers
    # @return an integer
    def firstMissingPositive(self, A):
        l = set(A)
        n = len(A)

		# Loop from [1.. n+1]
		# so if the array has all [1..n],
		# we'll continue looking for n+1
        for i in xrange(1, n+2):
            if i not in l:
                return i

		# Technically, we should never reach until here
		# However for the sake of completeness
		# return -1, indicating there might be an error
		return -1


if __name__ == '__main__':
	s = Solution()
	assert s.firstMissingPositive([1,2,0]) == 3
	assert s.firstMissingPositive([3,4,-1,1]) == 2
	assert s.firstMissingPositive([-8, -7, -6]) == 1
	assert s.firstMissingPositive([1,2,3,4,5]) == 6
	assert s.firstMissingPositive([1,5,2,4]) == 3
	assert s.firstMissingPositive([6,0,1,4,3,2]) == 5


