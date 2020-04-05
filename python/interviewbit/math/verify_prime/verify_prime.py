'''
https://www.interviewbit.com/problems/verify-prime/

Verify Prime

Given a number N, verify if N is prime or not.

Return 1 if N is prime, else return 0.

Example :
	Input : 7
	Output : True
'''


'''
Solution Outline:
	For a given number, n, if there's a divisor, a < sqrt(n), then its pair, b, (a*b == n)
	will be on the other side of sqrt(n), ie, b > sqrt(n)
	Except when n is a square number in which case a = b = sqrt(n)
	Check for all divisors <= sqrt(n), if we dont find any, n is prime
'''

class Solution:
	# @param n: Integer
	# @return an integer
	def isPrime(self, n):
		if n < 2:
			return 0

		# 2 is a prime number
		if n == 2:
			return 1
		elif n & 1 == 0:
			# every other even number
			return 0

		# Look for divisibility for all other odd numbers
		i = 3
		while i * i <= n:
			if n % i == 0:
				return 0
			i += 2

		return 1


if __name__ == '__main__':
	s = Solution()
	assert s.isPrime(1) == 0
	assert s.isPrime(2) == 1
	assert s.isPrime(4) == 0
	assert s.isPrime(3) == 1
	assert s.isPrime(5) == 1
	assert s.isPrime(7) == 1
	assert s.isPrime(9) == 0
	assert s.isPrime(10) == 0
	assert s.isPrime(101) == 1
	assert s.isPrime(1001) == 0 # 7 * 143

