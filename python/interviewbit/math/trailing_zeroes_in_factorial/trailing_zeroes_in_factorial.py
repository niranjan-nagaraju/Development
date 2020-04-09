#encoding: utf-8
'''
https://www.interviewbit.com/problems/trailing-zeros-in-factorial/

Trailing Zeros in Factorial

Given an integer n, return the number of trailing zeroes in n!.

Note: Your solution should be in logarithmic time complexity.

Example :
	n = 5
	n! = 120 
	Number of trailing zeros = 1
	So, return 1
'''

'''
Solution Outline:
	Factors of 5 contribute 1 zero, factors of 5² contribute an additional one (two total),
	  5³ contribute three in total, etc

	Count number of factors of 5, 25s, 125s, etc to get trailing zeroes in a factorial
'''

class Solution:
	def trailingZeroesInFactorial(self, n):
		count = 0
		while n:
			count += n/5
			n /= 5

		return count


if __name__ == '__main__':
	s = Solution()
	assert s.trailingZeroesInFactorial(0) == 0
	assert s.trailingZeroesInFactorial(5) == 1
	assert s.trailingZeroesInFactorial(6) == 1
	assert s.trailingZeroesInFactorial(4) == 0
	assert s.trailingZeroesInFactorial(20) == 4
	assert s.trailingZeroesInFactorial(25) == 6
	assert s.trailingZeroesInFactorial(26) == 6
	assert s.trailingZeroesInFactorial(100) == 24
	assert s.trailingZeroesInFactorial(124) == 28
	assert s.trailingZeroesInFactorial(125) == 31
	assert s.trailingZeroesInFactorial(126) == 31

