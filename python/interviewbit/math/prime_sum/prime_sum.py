#encoding: utf-8
'''
https://www.interviewbit.com/problems/prime-sum/

Prime Sum

Given an even number ( greater than 2 ), return two prime numbers whose sum will be equal to given number.

NOTE A solution will always exist. read Goldbach’s conjecture

Example:
	Input : 4
	Output: 2 + 2 = 4

	If there are more than one solutions possible, return the lexicographically smaller solution.

	If [a, b] is one solution with a <= b,
	and [c,d] is another solution with c <= d, then

	[a, b] < [c, d] 

	If a < c OR a==c AND b < d. 
'''

from collections import defaultdict
class Solution:
	def sieve(self, n):
		primes = [False, False] + [True] * (n-1) # 0 and 1 are striked out to begin with

		for i in xrange(2, n+1):
			if not primes[i]:
				continue

			j = i*i  # start with x²
			# strike out all factors of i
			while j < len(primes):
				primes[j] = False
				j += i

		return primes


	def prime_sum(self, n):
		primes = self.sieve(n)

		for i in xrange(2, n):
			if primes[i] and primes[n-i]:
				return [i, n-i]

		return -1



if __name__ == '__main__':
	s = Solution()
	assert s.prime_sum(4) == [2,2]
	assert s.prime_sum(22) == [3,19]
	assert s.prime_sum(48) == [5, 43]

