'''
https://leetcode.com/problems/reverse-integer/

Given a 32-bit signed integer, reverse digits of an integer.
'''

class Solution(object):
	def reverse(self, x):
		"""
		:type x: int
		:rtype: int
		"""


		sign = -1 if x < 0 else 1
		if sign == -1:
			x = -x

		reverse = 0
		i = 0
		while x:
			digit = x%10
			reverse = reverse * 10 + digit
			x = x / 10

		# 32-bit integer overflow => return 0
		if reverse > 2**31-1:
			return 0

		return (sign * reverse)



if __name__ == '__main__':
	s = Solution()
	assert s.reverse(123) == 321
	assert s.reverse(-321) == -123
	assert s.reverse(0) == 0
	assert s.reverse(-0) == 0
	assert s.reverse(-1) == -1
	assert s.reverse(1) == 1
	assert s.reverse(1234567809) == 0


