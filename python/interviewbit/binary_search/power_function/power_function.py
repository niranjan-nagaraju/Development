#encoding: utf-8
'''
https://www.interviewbit.com/problems/implement-power-function/

Implement Power Function

Implement pow(x, n) % d.

In other words, given x, n and d,

find (xⁿ % d)

Note that remainders on division cannot be negative.
In other words, make sure the answer you return is non negative.

Input : x = 2, n = 3, d = 3
Output : 2

2^3 % 3 = 8 % 3 = 2.
'''


'''
Solution Outline: O(log n) time
	Use binary exponentiation,
	  Use the binary representation of n
	  multiply by x, x², ... depending on if ith bit of n is set
	e.g., 3¹¹,
	11: 0b1011
	3¹¹ == 1*3⁸ * 0*3⁴ * 1*3² * 1*3¹

3² = 3¹ * 3¹
3⁴ = 3² * 3²
3⁸ = 3⁴ * 3⁴

For (xⁿ % d), Use %d for every multiplication
'''
class Solution:
	def power(self, x, n, d):
		if x==1 or x==0:
			return x

		res = 1
		while n:
			if n & 1:
				res = (res * x) % d
			x = (x*x) % d
			n >>= 1

		return res


if __name__ == '__main__':
	s = Solution()
	assert s.power(3, 5, 100) == 43 # 243%100
	assert s.power(2, 3, 3) == 2
	assert s.power(-2, 3, 3) == 1

