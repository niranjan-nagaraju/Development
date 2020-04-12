'''
https://www.interviewbit.com/problems/largest-coprime-divisor/

Largest Coprime Divisor

You are given two positive numbers A and B. You need to find the maximum valued integer X such that:
	X divides A i.e. A % X = 0
	X and B are co-prime i.e. gcd(X, B) = 1

For example,
	A = 30
	B = 12
	We return
	X = 5
'''

class Solution:
	def gcd(self, A, B):
		while B:
			r = A % B
			A = B
			B = r
		return A

	def find_largest_coprime_divisor(self, A, B):
		x = A
		while x > 0:
			if A % x == 0 and self.gcd(x, B) == 1:
				return x
			x -= 1

		return -1



if __name__ == '__main__':
	s = Solution()
	assert s.find_largest_coprime_divisor(30, 12) == 5
	assert s.find_largest_coprime_divisor(15, 3) == 5
	assert s.find_largest_coprime_divisor(14, 28) == 1
	assert s.find_largest_coprime_divisor(2, 3) == 2
	assert s.find_largest_coprime_divisor(90, 47) == 90

