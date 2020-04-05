'''
https://www.interviewbit.com/problems/all-factors/

All Factors

Given a number N, find all factors of N.

Example:
	N = 6 
	factors = {1, 2, 3, 6}
Make sure the returned array is sorted.
'''

'''
Solution Outline:
	For a given number, n, if there's a divisor, a < sqrt(n), then its pair, b, (a*b == n)
	will be on the other side of sqrt(n), ie, b > sqrt(n)
	Except when n is a square number in which case a = b = sqrt(n)
	
	So each time we add a divisor, a, we can also add b, its pair without having to enumerate it too
	and stop at divisors below sqrt(n)
'''
class Solution:
	# @param n : integer
	# @return a list of integers
	def allFactors(self, n):
		i = 1
		factors_1 = []
		factors_2 = []
		while i * i <= n:
			if n % i == 0:
				if n / i == i:
					factors_1.append(i)
				else:
					factors_1.append(i)
					factors_2.append(n/i)

			i += 1

		# factors_2, the divisors > sqrt(n) will be reverse sorted
		# reverse it and append it to factors_1 (divisors <= sqrt(n))
		return factors_1 + factors_2[::-1]


if __name__ == '__main__':
	s = Solution()
	assert s.allFactors(6) == [1,2,3,6]
	assert s.allFactors(9) == [1,3,9] 
	assert s.allFactors(16) == [1,2,4,8,16]
	assert s.allFactors(18) == [1,2,3,6,9,18]

