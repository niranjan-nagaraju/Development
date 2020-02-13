#encoding: utf-8
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

'''
Solution Outline: O(n) time, O(1) memory
	Let r and m be the repeated and missing numbers respectively.
	sigma(A) = sigma(n) + r - m
	  => r - m = (sigma(A) - sigma(n)) = x
	sigma(A²) = sigma(n²) + r² - m²
	  => r² - m² = sigma(A²) - sigma(n²) = y
	  => (r+m)(r-m) = y
	  => (r+m)(x) = y
	  => r+m = y/x
	r+m = y/x
	r-m = x
    => Add: 2r = (y/x)+x
	=> r = ((y/x) + x)/2
	=> m = r-x

NOTE: sigma(n): n(n+1)/2
      sigma(n²): n(n+1)(2n+1)/6
	    sigma(n²) / sigma(n) = n(n+1)(2n+1)/6 * 2/(n.(n+1) = (2n+1)/3
		sigma(n²) = sigma(n) * (2n+1)/3
'''

class Solution:
	# @param A : tuple of integers
	# @return a list of integers
	def repeatedNumber(self, A):
		n = len(A)

		sigma_A = sum(A)
		sigma_A_sq = sum(map(lambda x: x*x, A))
		sigma_n = n*(n+1)/2
		sigma_n_sq = sigma_n * (2*n+1)/3

		x = sigma_A - sigma_n
		y = sigma_A_sq - sigma_n_sq

		r = ((y/x) + x)/2
		m = r - x

		return [r, m]


if __name__ == '__main__':
	s = Solution()
	assert s.repeatedNumber([3,1,2,5,3]) == [3,4]
	assert s.repeatedNumber([1,2,3,4,5,6,4,8,9,10]) == [4, 7]




