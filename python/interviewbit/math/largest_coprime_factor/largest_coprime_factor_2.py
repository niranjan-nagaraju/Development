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


'''
Solution Outline:
	Let x: A
	Remove all common divisors of A, B from A by repeatedly dividing and replacing x by gcd(x, B)
	Return last x when gcd(x,B) reaches 1 {x is a divisor of A and coprime to B at this point, since we start at x=A, x is also maximized}


Sample run:
	A = 30
	B = 12

	x = 30
	gcd(30, 12) = 6
	x = 30/6 = 5

	gcd(5, 12) == 1
	return 5
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
		while self.gcd(x, B) > 1:
			x = x / self.gcd(x, B)

		return x



if __name__ == '__main__':
	s = Solution()
	assert s.find_largest_coprime_divisor(30, 12) == 5
	assert s.find_largest_coprime_divisor(15, 3) == 5
	assert s.find_largest_coprime_divisor(14, 28) == 1
	assert s.find_largest_coprime_divisor(2, 3) == 2
	assert s.find_largest_coprime_divisor(90, 47) == 90

