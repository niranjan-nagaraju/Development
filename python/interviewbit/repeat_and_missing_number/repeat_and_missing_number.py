'''
https://www.interviewbit.com/problems/repeat-and-missing-number-array/

Repeat and Missing Number Array

You are given a read only array of n integers from 1 to n.

Each integer appears exactly once except A which appears twice and B which is missing.

Return A and B.

Note: Your algorithm should have a linear runtime complexity. Could you implement it without using extra memory?

Note that in your output A should precede B.

Example:
Input:[3 1 2 5 3] 
Output:[3, 4] 
A = 3, B = 4
'''

class Solution:
	# @param A : tuple of integers
	# @return a list of integers
	def repeatedNumber(self, A):
		n = len(A)
		count = [0]*(n+1) # 1-indexed

		r = None
		for x in A:
			count[x] += 1
			if count[x] == 2:
				r = x

		for i in xrange(1, n+1):
			if count[i] == 0:
				return [r, i]

		return [None, None]



if __name__ == '__main__':
	s = Solution()
	assert s.repeatedNumber([3,1,2,5,3]) == [3,4]
	assert s.repeatedNumber([1,2,3,4,5,6,4,8,9,10]) == [4, 7]




