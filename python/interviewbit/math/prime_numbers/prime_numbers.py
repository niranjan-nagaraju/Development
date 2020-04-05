#encoding: utf-8
'''
https://www.interviewbit.com/problems/prime-numbers

Prime Numbers

Given a number N, find all prime numbers upto N ( N included ).

Example:
	if N = 7,
	all primes till 7 = {2, 3, 5, 7}

Make sure the returned array is sorted.
'''

class Solution:
	def sieve(self, n):
		primes = [-1, -1] + range(2, n+1) # 0 and 1 are striked out to begin with

		for x in primes[2:]:
			if x == -1:
				continue

			j = x*x  # start with x²
			# strike out all factors of i
			while j < len(primes):
				primes[j] = -1
				j += x

		return filter(lambda x : x > 0, primes)


if __name__ == '__main__':
	s = Solution()
	assert s.sieve(7) == [2, 3, 5, 7]
	assert s.sieve(18) == [2, 3, 5, 7, 11, 13, 17]
	assert s.sieve(19) == [2, 3, 5, 7, 11, 13, 17, 19]

