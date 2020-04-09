'''
https://www.interviewbit.com/problems/power-of-two-integers/

Given a positive integer which fits in a 32 bit signed integer, find if it can be expressed as A^P where P > 1 and A > 0. A and P both should be integers.

Example
Input : 4
Output : True  
as 2^2 = 4. 
'''

import math
class Solution:
	def is_power(self, n):
		if n == 1:
			return True

		i = 2
		while i * i <= n:
			x = math.log(n, i)
			if round(x, 4) == (int)(x) and math.pow(i, round(x,4)) == n:
				return True

			i += 1

		return False


if __name__ == '__main__':
	s = Solution()
	assert s.is_power(8) == True
	assert s.is_power(10) == False
	assert s.is_power(100) == True
	assert s.is_power(12) == False
	assert s.is_power(536870912) == True
	assert s.is_power(16808) == False

