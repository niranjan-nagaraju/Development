'''
https://www.interviewbit.com/problems/reverse-integer/

Reverse integer

Reverse digits of an integer.

Example1:
	x = 123,
	return 321

Example2:
	x = -123,
	return -321

Return 0 if the result overflows and does not fit in a 32 bit signed integer
'''

class Solution:
	# @param n : integer
	# @return an integer
	def reverse(self, n):
		negative = False
		if n < 0:
			negative = True
			n = -n

		reversed_n = 0
		while n:
			d = n % 10
			reversed_n = reversed_n * 10 + d
			n /= 10

		# restore sign
		if negative:
			reversed_n = -reversed_n
			if reversed_n < -(1<<31):
				return 0
		else:
			if reversed_n > (1<<31)-1:
				return 0

		return reversed_n


if __name__ == '__main__':
	s =  Solution()
	assert s.reverse(-12345) == -54321
	assert s.reverse(12345) == 54321
	assert s.reverse(1) == 1
	assert s.reverse(100) == 1
	assert s.reverse(2147483647) == 0
	assert s.reverse(7463847412) == 2147483647
	assert s.reverse(8463847412) == 0
	assert s.reverse(-8463847412) == -2147483648
	assert s.reverse(-9463847412) == 0

