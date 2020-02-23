#!/usr/bin/python

from Primes1mil import *

def isTruncatablePrime (num):
	orig_number = num
	digits = 1

	isTruncatable = True
	while (num > 0):
		isTruncatable = isTruncatable and (num in primes)
		num = num / 10
		digits *= 10
	
	digits /= 10

	num = orig_number
	while (digits > 0):
		isTruncatable = isTruncatable and (num in primes)
		num = orig_number % digits
		digits /= 10

	return isTruncatable


def get_truncatable_primes ():
	num_primes = 0
	trunc_primes = []
	for p in primes [4:] :
		if isTruncatablePrime(p):
			num_primes += 1
			trunc_primes.append(p
					)
		if num_primes == 11:
			return trunc_primes
	
	return trunc_primes

trunc_primes = get_truncatable_primes()
print trunc_primes
print sum(trunc_primes)

'''
[23, 37, 53, 73, 313, 317, 373, 797, 3137, 3797, 739397]
748317
'''
