#!/usr/bin/python

# Generate a sieve of Eratosthenes - a list of primes <= n
def sieve(n):
	theSieve = []

	# skip even numbers, for they are all divisible by 2 anyway
	for x in range(3, n+1, 2):
		# check if x is not divisible by every prime before it
		# if it is not, add it to the sieve
		if isPrime(theSieve, x):
			theSieve.append(x)

	# Don't forget 2 - the first and the only even prime number
	return [2] + theSieve


'''
Given a sieveList, say [3,5,7, ..]  and n > every element in sieveList,
Check if n is divisible by every element in sieveList, if it is not, n is a prime and can be added to sieveList 

NOTE: We don't need to check divisibility of n for every element in sieveList, just ones that are <= n/2
e.g., there really is no need to check if 10 is divisible by 9, 10/2==5 is 10's max divisor.
'''
def isPrime(sieveList, n):
	for x in sieveList:
		# n survived so far, we don't need to check divisibility of n
		# any further, as it would be redundant
		# e.g. [3,5,7], n = 11
		# n/2 = 5
		# we only need to check 11 % 3, and 11 % 5,
		# checking 11 % 7 is redundant as (11/7.0 < 2)
		if x > n/2:
			return True

		if n % x == 0:
			return False

	# This is technically not needed for anything other than n==3, as the sieve would be empty before it and therefore hit this.
	# But for all other n, we should have returned well before 
	# the sieveList is exhausted, but if we haven't, then divisibility test hasn't failed for every prime before 'n'
	# so it's *probably* (!) prime?!
	return True


if __name__ == '__main__':
	assert (sieve(100) ==  [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97])

